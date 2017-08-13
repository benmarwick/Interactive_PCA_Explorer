#!/bin/bash

# a one-liner to stop the docker container

docker ps | grep shiny-pca-maker | cut -f1 -d " " | xargs docker stop