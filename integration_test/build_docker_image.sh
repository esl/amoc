#!/bin/bash

# Get current repo version
OTP_VSN="${TRAVIS_OTP_RELEASE:-22.3.4}-1"
echo "ERLANG/OTP ${OTP_VSN}"

docker build \
	-f Dockerfile \
	-t amoc:latest \
	--build-arg otp_vsn=${OTP_VSN} \
	.
