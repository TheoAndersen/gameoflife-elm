#!/bin/bash

elm-make Test.elm --output raw-test.js
elm-stuff/packages/laszlopandy/elm-console/1.0.3/elm-io.sh raw-test.js test.js
node test.js
