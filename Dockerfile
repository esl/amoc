FROM public.ecr.aws/debian/debian:buster 

ARG DEBIAN_FRONTEND=noninteractive
ARG otp_vsn=18.3
ARG asdf_version=0.8.1
ARG HOME=/root

RUN apt-get --quiet update
RUN apt-get -q -y install ca-certificates \
                          procps \
                          git \
                          unzip \
                          curl \
                          build-essential \
                          libncurses5-dev \
                          libssl-dev \
                          unixodbc-dev

RUN git clone https://github.com/openssl/openssl.git /openssl --branch OpenSSL_1_0_2-stable
WORKDIR /openssl
RUN ./config --prefix=/openssl-1.0 shared -fPIC
RUN make depend && make && make install

RUN git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch "v${asdf_version}"
RUN echo '. ~/.asdf/asdf.sh' >> ~/.bashrc
ENV PATH "${HOME}/.asdf/shims:${HOME}/.asdf/bin:${PATH}"
RUN asdf plugin add erlang
RUN KERL_CONFIGURE_OPTIONS="--with-ssl=/openssl-1.0" asdf install erlang "${otp_vsn}"
RUN asdf global erlang "${otp_vsn}"

COPY . /amoc_build
WORKDIR /amoc_build

RUN git clean -ffxd
RUN make rel

RUN mkdir /amoc
RUN tar -C /amoc -zxf _build/demo/rel/amoc/amoc-2.2.1-OTP18.tar.gz
ENV PATH "/amoc/bin:${PATH}"

CMD ["amoc", "foreground"]

