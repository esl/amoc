-module(dummy_scenario).
-behaviour(amoc_scenario).

%% var1 must be equal to one of the values in the list and the default value is def1
-required_variable({var1, "description1", def1, [def1, another_value]}).

-required_variable([
    %% var2 must be positively verified by the test_verification_function/1 function.
    %% this function must be exported from the scenario module, or from any of
    %% the modules specified in the config_verification_modules amoc configuration
    %% variable.
    {var2, "description2", def2, test_verification_function},
    %% alternatively the verification function can be supplied as a function pointer
    %% in a 'fun module:function/arity' format. Note that it must be an exported function
    %% anyway, as 'fun function/arity' format in the module attribute simply will
    %% not pass compilation.
    {var3, "description3", def3, fun ?MODULE:test_verification_function/1}]).

%% none is a predefined verification function which accepts all the values
-required_variable({var4, "description4", def4, none}).

-required_variable([
    %% this is the same as {var5, "description5", def5, none}
    {var5, "description5", def5},
    %% this is the same as {var6, "description6", undefined, none}
    {var6, "description6"},
    {nodes,"this variable is set for docker container via AMOC_NODES env"},
    {test, "this one to be set via REST API"}]).

%% parameter verification method
-export([test_verification_function/1]).

%% amoc_scenario behaviour
-export([init/0,start/1]).

test_verification_function(def2) -> true;
test_verification_function(_)    -> {true, new_value}.

-spec init() -> ok.
init() ->
    %% amoc follows a couple of rules during the scenario initialisation:
    %%  - if any parameter verification fails, amoc will not start
    %%    the scenario and the init/0 function is not triggered.
    %%  - if the init/0 function fails, amoc will not start any users (by
    %%    calling a start/1 or start2 function)
    %% if the REST API reports that scenario is executed, than all the
    %% initialisation steps described above have passed successfully
    def1 = amoc_config:get(var1, def_value),
    def2 = amoc_config:get(var2, def_value),
    new_value = amoc_config:get(var3, def_value),
    def4 = amoc_config:get(var4, def_value),
    def5 = amoc_config:get(var5, def_value),
    def_value = amoc_config:get(var6, def_value),
    undefined = amoc_config:get(var6),
    [_|_] = amoc_config:get(nodes),
    %% it doesn't matter if an undeclared_variable is passed through the
    %% os or app environment variable. if it's not declared using the
    %% -required_variable(...) it is reported as undefined variable.
    def_value = amoc_config:get(undeclared_variable, def_value),
    undefined = amoc_config:get(undeclared_variable),
    <<"test_value">> = amoc_config:get(test),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(_Id) ->
    amoc_user:stop().
