# A Murder of Crows
[![](https://github.com/esl/amoc/workflows/CI/badge.svg)](https://github.com/esl/amoc/actions?query=workflow%3ACI)
[![Hex](http://img.shields.io/hexpm/v/amoc.svg)](https://hex.pm/packages/amoc)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/amoc/)
[![codecov](https://codecov.io/github/esl/amoc/graph/badge.svg?token=R1zXAjO7H7)](https://codecov.io/github/esl/amoc)

---

A Murder of Crows, aka amoc, is a simple framework for running massively parallel tests in a distributed environment.

It can be used as a `rebar3` dependency:

```erlang
{deps, [
    {amoc, "3.2.0"}
]}.
```

or in `mix`:

```elixir
defp deps() do
  [
    {:amoc, "~> 3.2"}
  ]
end
```

[MongooseIM](https://github.com/esl/MongooseIM) is continuously being load tested with Amoc.
All the XMPP scenarios can be found [here](https://github.com/esl/amoc-arsenal-xmpp).

---

In order to implement and run locally your scenarios, follow the chapters about
[developing](https://hexdocs.pm/amoc/scenario.html) and [running](https://hexdocs.pm/amoc/local-run.html)
a scenario locally.
Before [setting up the distributed environment](https://hexdocs.pm/amoc/distributed.html),
please read through the configuration overview.

To see the full documentation, see [hexdocs](https://hexdocs.pm/amoc).

You can also try with the livebook demo here:

[![Run in Livebook](https://livebook.dev/badge/v1/blue.svg)](https://livebook.dev/run?url=https%3A%2F%2Fgithub.com%2Fesl%2Famoc%2Fblob%2Fmaster%2Fguides%2Famoc_livebook.livemd)
