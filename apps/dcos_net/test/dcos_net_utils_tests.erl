-module(dcos_net_utils_tests).
-include_lib("eunit/include/eunit.hrl").

string_join_with_dot_test_() ->
    [
        ?_assertEqual(<<".">>, dcos_net_utils:string_join([<<"">>], <<".">>)),
        ?_assertEqual(<<"">>, dcos_net_utils:string_join([], <<"nope">>)),
        ?_assertEqual(<<"com">>, dcos_net_utils:string_join([<<"com">>], <<".">>)),
        ?_assertEqual(<<"amazon.com">>, dcos_net_utils:string_join([<<"amazon">>, <<"com">>], <<".">>)),
        ?_assertEqual(<<"amazon...com">>, dcos_net_utils:string_join([<<"amazon">>, <<"com">>], <<"...">>)),
        ?_assertEqual(<<"amazon。com">>, dcos_net_utils:string_join([<<"amazon">>, <<"com">>], <<"。">>)),
        ?_assertEqual(<<"amazon.com.">>, dcos_net_utils:string_join([<<"amazon">>, <<"com">>, <<"">>], <<".">>))
    ].
