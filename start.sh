#!/bin/bash

rebar compile 
erl -pa ebin -eval "application:start(pandemic)" 
