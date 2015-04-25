#!/bin/bash

cd lib
elixirc *.ex
cd ..
mix escript.build
