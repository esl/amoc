FROM phusion/baseimage
MAINTAINER Rafał Studnicki <rafal.studnicki@erlang-solutions.com>

ENV AMOC_VSN master

RUN useradd -ms /bin/bash amoc

RUN buildDeps=' \
    git \
    make \
    gcc \
    clang \
    libexpat1-dev \
    wget \
    iproute2 \
    ' && \
    apt-get update && \
    apt-get install -y --no-install-recommends $buildDeps && \
    wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i erlang-solutions_1.0_all.deb && \
    apt-get update && \
    apt-get install -y esl-erlang=1:18.3.4 && \
    git clone https://github.com/esl/amoc.git --branch ${AMOC_VSN} amoc_build && \
    cd amoc_build && \
    make rel && \
    apt-get purge -y --auto-remove $buildDeps esl-erlang && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* && \
    rm -rf erlang-solutions_1.0_all.deb && \
    cp -r /amoc_build/_rel/amoc /home/amoc/amoc && \
    chown -R amoc:amoc /home/amoc/amoc && \
    rm -rf /amoc_build

EXPOSE 4000

RUN mkdir /etc/service/amoc
ADD amoc.sh /etc/service/amoc/run
ADD config/vm.args /home/amoc/amoc/releases/0.9
ADD config/sys.config.template /
ADD run.sh /

CMD ["/run.sh"]
