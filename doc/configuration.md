## Configuration

Amoc is configured through OTP application environment variables that
can be set in `app.config` configuration file (see [rebar3 documentation](https://www.rebar3.org/docs/releases#section-overlays))
or using operating system environment variables (uppercase with prefix ``AMOC_``). note that operating system environment variables are evaluated as erlang code

Amoc supports the next generic configuration parameters:

* ``nodes`` - required for the distributed scenario execution, the list of nodes that should be clustered together:
    * default value - empty list (`[]`)
    * os env example: `AMOC_NODES="['amoc@amoc-1', 'amoc@amoc-2']"`
    * app.config example:  `{nodes,  ['amoc@amoc-1', 'amoc@amoc-2]}`                                  

* ``api_port`` - the port for the amoc REST interfaces:
    * default value - 4000
    * os env example: `AMOC_API_PORT="4000"`
    * app.config example:  `{api_port, 4000}`
                                      
* ``interarrival`` - a delay (in ms, for each node in the cluster independently) between creating processes
  for two consecutive users:
    * default value - 50 ms.
    * os env example: `AMOC_INTERARRIVAL="50"`
    * app.config example:  `{interarrival, 50}`

* ``extra_code_paths`` - a list of paths that should be included using `code:add_pathsz/1` interface
    * default value - empty list (`[]`)
    * os env example: `AMOC_EXTRA_CODE_PATHS='["/some/path", "/another/path"]'`
    * app.config example:  `{extra_code_paths, ["/some/path", "/another/path"]}`
    
* ``config_verification_modules`` - a list of modules with configuration verification function 
(see Scenario Configuration section):
    * default value - empty list (`[]`)
    * os env example: `AMOC_EXTRA_CODE_PATHS="[some_module, another_module]"`
    * app.config example:  `{config_verification_modules, [some_module, another_module]}`

In addition to that amoc_metrics supports the next configuration parameters:

* ``metrics_reporter`` - exometer reporter name (atom).
    * default value - `exometer_report_graphite`
    * os env example: `AMOC_METRICS_REPORTER="exometer_report_graphite"`
    * app.config example:  `{metrics_reporter, exometer_report_graphite}`
    
* ``metrics_preconfigured`` - preconfigured amoc metrics, list of {Type, Name} tuples, 
where are parameters passed to `amoc_metrics:init/2`:
    * default value - empty list (`[]`)
    * os env example: `AMOC_METRICS_PRECONFIGURED="[{gauge, some_metric}, {times, another_metric}]"`
    * app.config example:  `{metrics_preconfigured, [{gauge, some_metric}, {times, another_metric}]}`

If ``metrics_reporter`` is not initialised, it's possible to configure grafite reporter
using the next environment variables:

* ``graphite_host`` - grafite host address (string or `undefined`):
    * default value - `undefined` (amoc_metrics doesn't try to initialise ``metrics_reporter``)
    * os env example: `AMOC_GRAPHITE_HOST='"graphite"'`
    * app.config example:  `{graphite_host, "graphite"}`

* ``graphite_port`` - grafite port:
    * default value - `2003`
    * os env example: `AMOC_GRAPHITE_PORT='2003'`
    * app.config example:  `{graphite_port, 2003}`    
    
* ``graphite_prefix`` - grafite prefix:
    * default value - `net_adm:localhost()`
    * os env example: `AMOC_GRAPHITE_PREFIX='"amoc"'`
    * app.config example:  `{graphite_prefix, "amoc"}`    
       

In the same manner you can also define your own entries to configure the scenario.

The ``amoc_config:get/1`` and ``amoc_config:get/2`` interfaces can be used to get
parameters required for your scenario, however every scenario must declare (using
`-required_variable(...)` attributes) all the required parameters in advance. For more
information see example [scenario module](../integration_test/dummy_scenario.erl)


NB: the reason why `-required_variable(...)` preferred over the usual behaviour
callback is because orchestration tools can easily extract the attributes even
without the compilation. while configuration via callback, requires successful
compilation of the module. for example, having such a module:
```erlang
-module(example).
-include("some_unavailable_header.hrl").
-some_attr({"some", value}).
-some_attr([{another, "value"}, 
            {yet, <<"another">>, "value"}]).
```
we cannot compile it without ``some_unavailable_header.hrl`` file, but we still
can parse it and extract attributes:
```
Eshell V10.3  (abort with ^G)
1> c(example).
example.erl:2: can't find include file "some_unavailable_header.hrl"
error
2> {ok, AbstractForm} = epp:parse_file("example.erl", []).
{ok,[{attribute,1,file,{"example.erl",1}},
     {attribute,1,module,example},
     {error,{2,epp,{include,file,"some_unavailable_header.hrl"}}},
     {attribute,3,some_attr,{"some",value}}, 
     {attribute,4,some_attr,
                [{another,"value"},{yet,<<"another">>,"value"}]}, 
     {eof,6}]}
3> lists:flatten([Value || {attribute, _, some_attr, Value} <- AbstractForm]).
[{"some",value},
 {another,"value"},
 {yet,<<"another">>,"value"}]

```
