## Configuration

Amoc is configured through environment variables (uppercase with prefix `AMOC_`).
Note that by default, environment variables values are deserialized as Erlang terms. This behavior can be customized by providing an alternative parsing module in the `config_parser_mod` configuration parameter for the amoc application. It can be done using the [application:set_env/4](https://www.erlang.org/doc/man/application#set_env-4) interface or via a [config file](https://www.erlang.org/doc/man/config). The custom parsing module must implement the `amoc_config_env` behavior.

Amoc supports the following generic configuration parameters:

* `nodes` - required for the distributed scenario execution, a list of nodes that should be clustered together:
    * default value - empty list (`[]`)
    * example: `AMOC_NODES="['amoc@amoc-1', 'amoc@amoc-2']"`


* `interarrival` - a delay (in ms, for each node in the cluster independently) between creating the processes
  for two consecutive users:
    * default value - 50 ms.
    * example: `AMOC_INTERARRIVAL="50"`
    * this parameter can be updated at runtime (in the same way as scenario configuration).

* `extra_code_paths` - a list of paths that should be included using `code:add_pathsz/1` interface
    * default value - empty list (`[]`)
    * example: `AMOC_EXTRA_CODE_PATHS='["/some/path", "/another/path"]'`

In the same manner you can also define your own entries to configure the scenario.

## Required Variables

The `amoc_config:get/1` and `amoc_config:get/2` interfaces can be used to get
parameters required for your scenario, however every scenario must declare (using
`-required_variable(...)`/`@required_variable ...` attributes) all the required parameters in advance.
For more information, see the example `dummy_scenario` in integration tests.

Scenario configuration also can be set/updated at runtime using an API.

```erlang
-required_variable(
    #{name => Name, description => Description,
      default_value => Value,
      update => UpdateMfa,
      verification => VerificationMfa}
  ).
```

```elixir
  ## Note that the attribute needs to be marked as persisted
  ## for the Elixir compiler to store it in the generated BEAM file.
  Module.register_attribute(__MODULE__, :required_variable, accumulate: true, persist: true)

  @required_variable %{
    name: name,
    description: description,
    default_value: 6,
    update: updated_mfa,
    verification: verification_mfa
  }
```

where

### `name`

* **Syntax:** atom
* **Example:** `var1`
* **Default:** this field is mandatory

### `description`

* **Syntax:** A string describing how this variable is used, can be extracted by APIs to document the behavior
* **Example:** `"a description of this variable"`
* **Default:** this field is mandatory

### `default_value`

* **Syntax:** value of the expected type
* **Example:** `10`
* **Default:** `undefined`

### `verification`

* **Syntax:** `none`, a list of allowed values, or an `mfa` of arity `1`
* **Example:** `{?MODULE, is_binary, 1}`
* **Default:** `none`

A verification function that will check the given value is correct. It is trigger for verifying the initial values, including the default value, and before updated values are applied.
- If it is set to `none`, all values are allowed.
- If it is set to a list of values, any given value checks that the new value is in such allowlist.
- If it is an `mfa`, the given function will be called on the given value. This function
must be pure and return a boolean or a `{true, NewValue} | {false, Reason}`. It can also be used for preprocessing of the input value by returning `{true, NewValue}`.

### `update`

* **Syntax:** `read_only`, `none`, or an `mfa` of arity 2
* **Example:** `{?MODULE, update, 2}`
* **Default:** `read_only`

An action to take when the value of this variable is updated. It is triggered at runtime when updates to the value are applied.
- If it is set to `read_only`, updates will fail.
- If it is set to `none`, all updates are allowed.
- If it is an `mfa`, the given function will be called on the old and new value.

## Rationale

The reason why the `-required_variable(...)` is preferred over the usual behavior
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

cannot be compiled without the `some_unavailable_header.hrl` file, but we still
can parse it and extract the attributes:

```
Eshell V14.0 (press Ctrl+G to abort, type help(). for help)
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
