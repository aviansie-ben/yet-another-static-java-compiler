#!/bin/bash

cd $(dirname $(realpath "$0"))/out
set -e

cargo run --manifest-path ../../compiler/Cargo.toml -- .:../../jcl/out:/usr/lib/jvm/java-8-openjdk/jre/lib/rt.jar "$1"
llc -O2 test.bc
clang -no-pie -L../../rt/target/debug -lmocha_rt test.s -o test

