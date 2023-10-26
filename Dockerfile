ARG otp_vsn=25.3
FROM erlang:${otp_vsn} AS builder
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

WORKDIR /amoc_build

COPY ./rebar.config ./rebar.lock ./Makefile ./
RUN make deps

COPY ./rel rel
COPY ./src src

RUN make rel

ENV PATH "/amoc_build/_build/default/rel/amoc/bin:${PATH}"

CMD ["amoc", "foreground"]
