ARG otp_vsn=25.3
FROM erlang:${otp_vsn}-slim AS builder
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

WORKDIR /amoc_build

COPY ./rebar.config ./rebar.lock ./
RUN rebar3 deps && rebar3 compile -d

COPY ./integration_test integration_test
COPY ./scenarios scenarios
COPY ./priv priv
COPY ./rel rel
COPY ./src src

RUN rebar3 as demo release

ENV PATH "/amoc_build/_build/demo/rel/amoc/bin:${PATH}"

CMD ["amoc", "foreground"]
