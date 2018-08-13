#!/bin/bash

erl -pa ./_build/default/lib/queens/ebin -noinput -run queens main 15 1
