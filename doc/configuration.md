## Configuration

Amoc is configured through the environment variables (uppercase with prefix ``AMOC_``).
Note that the environment variables are evaluated as erlang code

Amoc supports the following generic configuration parameters:

* ``nodes`` - required for the distributed scenario execution, a list of nodes that should be clustered together:
    * default value - empty list (`[]`)
    * example: `AMOC_NODES="['amoc@amoc-1', 'amoc@amoc-2']"`

* ``api_port`` - a port for the amoc REST interfaces:
    * default value - 4000
    * example: `AMOC_API_PORT="4000"`
                                      
* ``interarrival`` - a delay (in ms, for each node in the cluster independently) between creating the processes
  for two consecutive users:
    * default value - 50 ms.
    * example: `AMOC_INTERARRIVAL="50"`
    * this parameter can be updated runtime (in the same way as scenario configuration).

* ``extra_code_paths`` - a list of paths that should be included using `code:add_pathsz/1` interface
    * default value - empty list (`[]`)
    * example: `AMOC_EXTRA_CODE_PATHS='["/some/path", "/another/path"]'`

In addition to that, `amoc_metrics` support the following configuration parameters:

* ``metrics_reporter`` - exometer reporter name (atom).
    * default value - `exometer_report_graphite`
    * example: `AMOC_METRICS_REPORTER="exometer_report_graphite"`

If the ``metrics_reporter`` is not initialised, it's possible to configure a Graphite reporter
using the following environment variables:

* ``graphite_host`` - a graphite host address (string or `undefined`):
    * default value - `undefined` (amoc_metrics do not try to initialise a ``metrics_reporter``)
    * example: `AMOC_GRAPHITE_HOST='"graphite"'`

* ``graphite_port`` - graphite port:
    * default value - `2003`
    * example: `AMOC_GRAPHITE_PORT='2003'`
    
* ``graphite_prefix`` - graphite prefix:
    * default value - `net_adm:localhost()`
    * example: `AMOC_GRAPHITE_PREFIX='"amoc"'`

In order to initialise some preconfigured metrics, other applications can declare
the `predefined_metrics` environment variable (in their own `*.app.src` file):  
```
{predefined_metrics, [{gauge, some_metric}, {times, another_metric}]}
```

In the same manner you can also define your own entries to configure the scenario.

The ``amoc_config:get/1`` and ``amoc_config:get/2`` interfaces can be used to get
parameters required for your scenario, however every scenario must declare (using
`-required_variable(...)` attributes) all the required parameters in advance. For more
information, see the example [scenario module](../integration_test/dummy_scenario.erl)

Scenario configuration also can be set/updated runtime using REST API.

NB: the reason why the `-required_variable(...)` is preferred over the usual behaviour
callback is because the orchestration tools can easily extract the attributes even
without the compilation, while configuring via a callback, requires a successful
compilation of the module. As an example, a module:
```erlang
-module(example).
-include("some_unavailable_header.hrl").
-some_attr({"some", value}).
-some_attr([{another, "value"}, 
            {yet, <<"another">>, "value"}]).
```
cannot be compiled without the ``some_unavailable_header.hrl`` file, but we still
can parse it and extract the attributes:
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
