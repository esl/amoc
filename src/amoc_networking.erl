-module(amoc_networking).

-export([get_iffaddrs_by_subnets/1]).

-spec get_iffaddrs_by_subnets([inet:ip_address()]) ->
    [inet:ip_address()].
get_iffaddrs_by_subnets(Subnets) ->
    {ok, IFs} = inet:getifaddrs(),
    IFs2 = [ unpack_interfaces(IF) || IF <- IFs],
    IFs3 = lists:flatten(IFs2),
    Result = lists:filter(
               fun ({Addr, Mask}) ->
                       lists:member(get_subnet(Addr, Mask), Subnets)
               end,
               IFs3),
    [ X || {X, _} <- Result].

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec unpack_interfaces(proplists:property()) ->
    [{inet:ip_address(), inet:ip_address()}].
unpack_interfaces({_IFName, Props}) ->
    Addrs = [ X || {addr, X} <- proplists:lookup_all(addr, Props)],
    Masks = [ X || {netmask, X} <- proplists:lookup_all(netmask, Props)],
    lists:zip(Addrs, Masks).

-spec get_subnet(inet:ip_address(), inet:ip_address()) ->
    inet:ip_address().
get_subnet({A, B, C, D}, {MA, MB, MC, MD}) ->
    {A band MA, B band MB, C band MC, D band MD};

get_subnet({A, B, C, D, E, F, G, H},
           {MA, MB, MC, MD, ME, MF, MG, MH}) ->
    {A band MA, B band MB, C band MC, D band MD,
     E band ME, F band MF, G band MG, H band MH}.
