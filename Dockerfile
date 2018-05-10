FROM phusion/baseimage
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

RUN useradd -ms /bin/bash amoc

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        git \
        make \
        gcc \
        clang \
        libexpat1-dev \
        wget \
        iproute2 && \
    wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i erlang-solutions_1.0_all.deb && \
    apt-get update && \
    apt-get install -y esl-erlang=1:18.3.4

COPY . /amoc_build

RUN cd amoc_build && \
    make rel && \
    rm _build/default/rel/amoc/*tar.gz

FROM phusion/baseimage
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

RUN useradd -ms /bin/bash amoc

COPY --chown=amoc:amoc --from=0 /amoc_build/_build/default/rel/amoc/ /home/amoc/amoc/

EXPOSE 4000

RUN mkdir /etc/service/amoc
ADD docker/amoc.sh /etc/service/amoc/run
ADD docker/config/vm.args /home/amoc/amoc/releases/0.9.0/
ADD docker/config/sys.config.template /
ADD docker/run.sh /

CMD ["/run.sh"]
