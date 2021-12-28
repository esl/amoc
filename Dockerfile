FROM phusion/baseimage:focal-1.0.0 as base-image
FROM base-image as amoc-build

RUN apt-get update
RUN apt-get -y install gnupg2

ARG DEBIAN_FRONTEND=noninteractive
ARG otp_vsn=24.0
ARG rebar_vsn=3.16.1

ADD https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb /tmp/
RUN dpkg -i /tmp/erlang-solutions_2.0_all.deb
RUN apt-get update

RUN apt-get -y install esl-erlang=1:${otp_vsn}-1

RUN apt-get -y install git make wget

COPY . /amoc_build

ADD https://github.com/erlang/rebar3/releases/download/${rebar_vsn}/rebar3 /bin
RUN chmod u+x /bin/rebar3

RUN cd /amoc_build && \
    git clean -ffxd && \
    make rel

FROM base-image

RUN useradd -ms /bin/bash amoc

COPY --from=amoc-build /amoc_build/_build/demo/rel/amoc/ /home/amoc/amoc/
COPY --from=amoc-build /amoc_build/scenarios /amoc_build/scenarios

# It seems hub.docker.com does not support --chown param to COPY directive
RUN chown -R amoc:amoc /home/amoc/amoc

EXPOSE 4000

RUN mkdir /etc/service/amoc
COPY docker/amoc.sh /etc/service/amoc/run

CMD ["/sbin/my_init"]
