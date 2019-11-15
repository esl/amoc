FROM phusion/baseimage as amoc-build
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

ARG otp_vsn=21.3.8.7-1

RUN useradd -ms /bin/bash amoc

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        git \
        make \
        gcc \
        g++ \
        clang \
        libexpat1-dev \
        wget \
        iproute2 && \
    wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i erlang-solutions_1.0_all.deb && \
    apt-get update && \
    apt-get install -y esl-erlang=1:${otp_vsn}

COPY . /amoc_build

RUN cd amoc_build && \
    git clean -ffxd && \
    make rel

FROM phusion/baseimage
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

RUN useradd -ms /bin/bash amoc

COPY --from=amoc-build /amoc_build/_build/prod/rel/amoc/ /home/amoc/amoc/
# It seems hub.docker.com does not support --chown param to COPY directive
RUN chown -R amoc:amoc /home/amoc/amoc

EXPOSE 4000

RUN mkdir /etc/service/amoc
ADD docker/amoc.sh /etc/service/amoc/run

CMD ["/sbin/my_init"]
