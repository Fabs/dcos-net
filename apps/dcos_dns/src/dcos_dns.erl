-module(dcos_dns).

-include("dcos_dns.hrl").
-include_lib("dns/include/dns_records.hrl").
-include_lib("erldns/include/erldns.hrl").

%% API
-export([
    family/1,
    resolve/1,
    resolve/2,
    resolve/3,
    get_leader_addr/0,
    init_metrics/0
]).

%% DNS Zone functions
-export([
    ns_record/1,
    soa_record/1,
    dns_record/2,
    dns_records/2,
    srv_record/2,
    cname_record/2,
    push_zone/2,
    push_prepared_zone/2
]).

-spec family(inet:ip_address()) -> inet | inet6.
family(IP) when size(IP) == 4 ->
    inet;
family(IP) when size(IP) == 8 ->
    inet6.

-spec(get_leader_addr() -> {ok, inet:ip_address()} | {error, term()}).
get_leader_addr() ->
    case resolve(<<"leader.mesos">>, inet) of
        {ok, [IP]} ->
            {ok, IP};
        {ok, _IPs} ->
            {error, not_found};
        {error, Error} ->
            {error, Error}
    end.

-spec(resolve(binary()) -> {ok, [inet:ip_address()]} | {error, term()}).
resolve(DNSName) ->
    resolve(DNSName, inet).

-spec(resolve(binary(), inet | inet6) ->
    {ok, [inet:ip_address()]} | {error, term()}).
resolve(DNSName, Family) ->
    resolve(DNSName, Family, 5000).

-spec(resolve(binary(), inet | inet6, timeout()) ->
    {ok, [inet:ip_address()]} | {error, term()}).
resolve(DNSName, Family, Timeout) ->
    DNSNameStr = binary_to_list(DNSName),
    ParseFun =
        case Family of
            inet -> fun inet:parse_ipv4_address/1;
            inet6 -> fun inet:parse_ipv6_address/1
        end,
    case ParseFun(DNSNameStr) of
        {ok, IP} ->
            {ok, [IP]};
        {error, einval} ->
            imp_resolve(DNSName, Family, Timeout)
    end.

-spec(imp_resolve(binary(), inet | inet6, timeout()) ->
    {ok, [inet:ip_address()]} | {error, term()}).
imp_resolve(<<"localhost">>, inet, _Timeout) ->
    {ok, [{127, 0, 0, 1}]};
imp_resolve(DNSName, Family, Timeout) ->
    Type =
        case Family of
            inet -> ?DNS_TYPE_A;
            inet6 -> ?DNS_TYPE_AAAA
        end,
    Query = #dns_query{name = DNSName, type = Type},
    Message = #dns_message{rd = true, qc = 1, questions = [Query]},
    Request = dns:encode_message(Message),
    case dcos_dns_handler:resolve(udp, Request, Timeout) of
        {ok, Response} ->
            try dns:decode_message(Response) of #dns_message{answers = RRs} ->
                DataRRs = [D || #dns_rr{data = D} <- RRs],
                {ok, [IP || #dns_rrdata_a{ip = IP} <- DataRRs] ++
                     [IP || #dns_rrdata_aaaa{ip = IP} <- DataRRs]}
            catch Class:Error ->
                {error, {Class, Error}}
            end;
        {error, Error} ->
            {error, Error}
    end.

%%%===================================================================
%%% DNS Zone functions
%%%===================================================================

-spec(soa_record(dns:dname()) -> dns:dns_rr()).
soa_record(Name) ->
    #dns_rr{
        name = Name,
        type = ?DNS_TYPE_SOA,
        ttl = 5,
        data = #dns_rrdata_soa{
            mname = <<"ns.spartan">>, %% Nameserver
            rname = <<"support.mesosphere.com">>,
            serial = 1,
            refresh = 60,
            retry = 180,
            expire = 86400,
            minimum = 1
        }
    }.

-spec(ns_record(dns:dname()) -> dns:dns_rr()).
ns_record(Name) ->
    #dns_rr{
        name = Name,
        type = ?DNS_TYPE_NS,
        ttl = 3600,
        data = #dns_rrdata_ns{
            dname = <<"ns.spartan">>
        }
    }.

-spec(dns_records(dns:dname(), [inet:ip_address()]) -> [dns:dns_rr()]).
dns_records(DName, IPs) ->
    [dns_record(DName, IP) || IP <- IPs].

-spec(dns_record(dns:dname(), inet:ip_address()) -> dns:dns_rr()).
dns_record(DName, IP) ->
    {Type, Data} =
        case dcos_dns:family(IP) of
            inet -> {?DNS_TYPE_A, #dns_rrdata_a{ip = IP}};
            inet6 -> {?DNS_TYPE_AAAA, #dns_rrdata_aaaa{ip = IP}}
        end,
    #dns_rr{name = DName, type = Type, ttl = ?DCOS_DNS_TTL, data = Data}.

-spec(srv_record(dns:dname(), {dns:dname(), inet:port_number()}) -> dns:rr()).
srv_record(DName, {Host, Port}) ->
    #dns_rr{
        name = DName,
        type = ?DNS_TYPE_SRV,
        ttl = ?DCOS_DNS_TTL,
        data = #dns_rrdata_srv{
            target = Host,
            port = Port,
            weight = 1,
            priority = 1
        }
    }.

-spec(cname_record(dns:dname(), dns:dname()) -> dns:dns_rr()).
cname_record(CName, Name) ->
    #dns_rr{
        name = CName,
        type = ?DNS_TYPE_CNAME,
        ttl = 5,
        data = #dns_rrdata_cname{dname=Name}
    }.

-spec(push_zone(dns:dname(), [dns:dns_rr()]) ->
    ok | {error, Reason :: term()}).
push_zone(ZoneName, Records) ->
    Records0 = [ns_record(ZoneName), soa_record(ZoneName) | Records],
    push_prepared_zone(ZoneName, Records0).

-spec(push_prepared_zone(dns:dname(), [dns:dns_rr()]) ->
    ok | {error, Reason :: term()}).
push_prepared_zone(ZoneName, Records) ->
    try erldns_zone_cache:get_zone_with_records(ZoneName) of
        {ok, #zone{records=Records}} ->
            ok;
        _Other ->
            Hash = crypto:hash(sha, term_to_binary(Records)),
            case erldns_zone_cache:put_zone({ZoneName, Hash, Records}) of
                ok ->
                    lager:notice(
                        "DNS Zone ~s was updated (~p records, sha: ~s)",
                        [ZoneName, length(Records), bin_to_hex(Hash)]),
                    prometheus_gauge:set(dns, zone_records, [ZoneName], length(Records)),
                    ok;
                {error, Error} ->
                    lager:error(
                        "Failed to push DNS Zone \"~s\": ~p",
                        [ZoneName, Error]),
                    {error, Error}
            end
    catch error:Error ->
        lager:error(
            "Failed to push DNS Zone \"~s\": ~p",
            [ZoneName, Error]),
        {error, Error}
    end.

-spec(bin_to_hex(binary()) -> binary()).
bin_to_hex(Bin) ->
    Bin0 = << <<(integer_to_binary(N, 16))/binary>> || <<N:4>> <= Bin >>,
    cowboy_bstr:to_lower(Bin0).

%%%===================================================================
%%% Metrics functions
%%%===================================================================

-spec(init_metrics() -> ok).
init_metrics() ->
    prometheus_gauge:new([
        {registry, dns},
        {name, zone_records},
        {labels, [zone]},
        {help, "Number of DNS records"}]).
