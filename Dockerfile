ARG otp_vsn=25.3
FROM erlang:${otp_vsn} AS builder
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

WORKDIR /amoc

COPY ./ ./
RUN git clean -ffxd

RUN make rel

ENV PATH "/amoc/_build/default/rel/amoc/bin:${PATH}"

CMD ["amoc", "foreground"]
