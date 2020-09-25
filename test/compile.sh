#!/bin/bash

cd $(dirname $(realpath "$0"))/out
set -e

cargo run --manifest-path ../../compiler/Cargo.toml -- .:../../jcl/out:/usr/lib/jvm/java-8-openjdk/jre/lib/rt.jar "$1"
llc -O0 test.bc

# Currently, LLVM seems to sometimes emit debuginfo pointing at line 0 for instructions that are
# materialized internally during code generation, e.g. during register allocation. This can cause
# gdb to get confused and think that the function has no debuginfo available after single-stepping
# into such code, so remove these before linking.
sed -i '/\.loc\s\+[0-9]\+\s\+0\s\+0/d' test.s

clang -no-pie -L../../rt/target/debug -lmocha_rt test.s -o test
