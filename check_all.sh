#!/bin/bash

set -ex

elm-make --yes

cd examples
elm-make --yes AllExamples.elm
open index.html
