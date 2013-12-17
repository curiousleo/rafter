#!/bin/sh

erlc -pa deps/eqc/ebin -I deps -o ebin src/*.erl test/*.erl
erl -pa ebin -pa deps/eqc/ebin -I deps -eval 'eqc:module(deque_eqc).' -s init stop -noshell
