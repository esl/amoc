-module(dummy_scenario).
-behaviour(amoc_scenario).

%% var1 must be equal to one of the values in the verification list
%% and the default value is def1
-required_variable(#{name => var1, description => "description1",
                     default_value => def1,
                     verification => [def1, another_value]}).

-required_variable([
    %% var2 must be positively verified by the test_verification_function/1 function.
    %% verification function must be supplied as a function pointer in a
    %% 'fun module:function/arity' format. Note that it must be an exported function.
    %% usage of 'fun function/arity' format in the module attribute simply will
    %% not pass compilation.
    #{name => var2, description => "description2", default_value => def2,
      verification => fun ?MODULE:test_verification_function/1},
    #{name => var3, description => "description3", default_value => def3,
      verification => fun ?MODULE:test_verification_function/1}]).

%% 'none' is a predefined verification function which accepts all the values
-required_variable(#{name => var4, description => "description4",
                     default_value => def4,
                     verification => none}).

-required_variable([
    %% when verification method is not set, it defaults to `none`
    #{name => var5, description => "description5", default_value => def5},
    %% when value is not set, it defaults to `undefined`
    #{name => var6, description => "description6"},
    #{name => nodes, description => "this variable is set for docker "
                                    "container via AMOC_NODES env"},
    #{name => test, description => "this one to be set via REST API"}]).

%% parameter verification method
-export([test_verification_function/1]).

%% amoc_scenario behaviour
-export([init/0, start/1]).

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
    [_ | _] = amoc_config:get(nodes),
    %% it doesn't matter if an undeclared_variable is passed through the
    %% os or erlang app environment variable. if it's not declared using
    %% the -required_variable(...) attribute, then any attempt to get it
    %% results in exception.
    {invalid_setting, undeclared_variable} =
        (catch amoc_config:get(undeclared_variable)),
    %% this variable is set via REST API
    <<"test_value">> = amoc_config:get(test),
    %% dummy_var variable is defined in the dummy_helper module.
    %% if dummy_helper is not propagated, then this call crashes
    default_value = amoc_config:get(dummy_var),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(_Id) ->
    %%sleep 15 minutes
    timer:sleep(1000 * 60 * 15),
    amoc_user:stop().
