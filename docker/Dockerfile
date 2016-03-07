FROM phusion/baseimage
MAINTAINER Rafał Studnicki <rafal.studnicki@erlang-solutions.com>

ENV AMOC_VSN master

RUN useradd -ms /bin/bash amoc

RUN apt-get update && \
    apt-get install -y git \
                       make \
                       gcc \
                       libexpat1-dev \
                       wget && \
    wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i erlang-solutions_1.0_all.deb && \
    apt-get update && \
    apt-get install -y esl-erlang=1:17.5.3 && \
    apt-get autoremove -y --purge && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* && \
    rm -rf erlang-solutions_1.0_all.deb && \
    git clone https://github.com/esl/amoc.git --branch ${AMOC_VSN} amoc_build && \
    cd amoc_build && \
    make rel && \
    cp -r /amoc_build/_rel/amoc /home/amoc/amoc && \
    chown -R amoc:amoc /home/amoc/amoc && \
    rm -rf /amoc_build

RUN mkdir /etc/service/amoc
ADD amoc.sh /etc/service/amoc/run

EXPOSE 4369 5000-5030

CMD ["/sbin/my_init"]
