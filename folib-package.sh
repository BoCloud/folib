#!/usr/bin/env bash
set -ex
mvn clean --settings folib-settings.xml -Dmaven.test.skip=true
cd folib-web-vue
yarn install
yarn run build
cd ..
mvn  package --settings folib-settings.xml -DskipTests

