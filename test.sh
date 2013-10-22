#!/bin/sh

erlc -pa lib/eqc-1.0.1/ebin -o ebin src/*.erl test/*.erl
erl -pa ebin -pa lib/eqc-1.0.1/ebin -eval 'eqc:module(deque_eqc).' -s init stop -noshell
