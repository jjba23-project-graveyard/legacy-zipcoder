#!/bin/sh 

# --test-show-details=always --format=progress --randomize

stack test  --test-arguments='--format=checks --color --randomize --print-cpu-time'\
  --coverage
