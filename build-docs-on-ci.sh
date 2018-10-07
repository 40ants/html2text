#!/bin/bash

set -e

# we only want to build documentation from the "reblocks" branch
# and want to make it only once.
# Also, we need to check if TRAVIS_PULL_REQUEST=false because Travis
# builds which are running for pull requests will have
# TRAVIS_BRANCH=reblocks, but TRAVIS_PULL_REQUEST_BRANCH=the-branch
# and TRAVIS_PULL_REQUEST=42 where 42 is a pull request number
if [ "$TRAVIS_BRANCH" = "fixing-travis" -a "$TRAVIS_PULL_REQUEST" = "false" -a "$LISP" = "ccl" -a "$TRAVIS_OS_NAME" = "linux" ]; then
    virtualenv env
    source env/bin/activate
    pip --version
    pip install pyopenssl
    pip install -r docs/requirements.txt
    ./build-docs.ros
else
    echo "Skipping documentation build because environment is not suitable."
fi
