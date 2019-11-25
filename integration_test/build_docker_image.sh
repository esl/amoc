#!/bin/bash

# Get current repo version
VERSION=$(git rev-parse --short HEAD)
OTP_VSN="${TRAVIS_OTP_RELEASE:-21.3.8.7}-1"
echo "ERLANG/OTP ${OTP_VSN}"

docker build \
	-f Dockerfile \
	-t amoc:latest \
	--build-arg vsn=${VERSION} \
	--build-arg otp_vsn=${OTP_VSN} \
	.
