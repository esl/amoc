#!/bin/bash

# Get current repo version
VERSION=$(git rev-parse --short HEAD)
ERLANG_VSN=${OTP_VSN:-21.3.8.7-1}
echo "ERLANG/OTP ${ERLANG_VSN}"

docker build \
	-f Dockerfile \
	-t amoc:latest \
	--build-arg vsn=${VERSION} \
	--build-arg otp_vsn=${ERLANG_VSN} \
	.
