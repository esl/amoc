FROM phusion/baseimage:focal-1.0.0 as amoc-build

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
                    git make wget gnupg

ARG otp_vsn=24.0-1

RUN wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb && \
    dpkg -i erlang-solutions_2.0_all.deb && \
    apt-get update && \
    apt-get install -y esl-erlang=1:${otp_vsn}

COPY . /amoc_build

RUN cd amoc_build && \
    git clean -ffxd && \
    make rel

FROM phusion/baseimage:focal-1.0.0
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

RUN useradd -ms /bin/bash amoc

COPY --from=amoc-build /amoc_build/_build/prod/rel/amoc/ /home/amoc/amoc/
# It seems hub.docker.com does not support --chown param to COPY directive
RUN chown -R amoc:amoc /home/amoc/amoc

EXPOSE 4000

RUN mkdir /etc/service/amoc
ADD docker/amoc.sh /etc/service/amoc/run

CMD ["/sbin/my_init"]
