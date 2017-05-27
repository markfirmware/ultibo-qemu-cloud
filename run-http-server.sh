#!/bin/bash
set -x

while [[ true ]]
do
    nohup python -m SimpleHTTPServer 80
    sleep 10
done
