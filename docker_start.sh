#!/bin/bash

# a rudimentary script to start a docker container running shiny-server
# pointing to the shiny-pca-maker app

APPDIR=$(cd "$(dirname "$0")"; pwd)
echo $APPDIR

docker run -d -p 3838:3838 -v $APPDIR:/srv/shiny-server jgbaum/shiny-pca-maker