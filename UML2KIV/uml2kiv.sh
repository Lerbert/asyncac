#! /bin/bash

docker run --rm -v "$(pwd):/work" uml2kiv "$@"
