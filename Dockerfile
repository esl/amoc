FROM phusion/baseimage
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

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
    apt-get install -y esl-erlang=1:21.2.2-1

COPY . /amoc_build

RUN cd amoc_build && \
    make rel && \
    rm _build/default/rel/amoc/*tar.gz

FROM phusion/baseimage
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

RUN useradd -ms /bin/bash amoc

COPY --from=0 /amoc_build/_build/default/rel/amoc/ /home/amoc/amoc/
# It seems hub.docker.com does not support --chown param to COPY directive
RUN chown -R amoc:amoc /home/amoc/amoc

EXPOSE 4000

RUN mkdir /etc/service/amoc
ADD docker/amoc.sh /etc/service/amoc/run
ADD docker/config/vm.args /home/amoc/amoc/releases/0.9.0/
ADD docker/config/sys.config.template /
ADD docker/run.sh /

CMD ["/run.sh"]
