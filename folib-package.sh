#!/usr/bin/env bash
set -ex
mvn clean --settings folib-settings.xml -Dmaven.test.skip=true -Dmaven.wagon.http.ssl.insecure=true -Dmaven.wagon.http.ssl.allowall=true -Dmaven.wagon.http.ssl.ignore.validity.dates=true
cd folib-web-vue
yarn install
yarn run build
cd ..
mvn  package --settings folib-settings.xml -DskipTests -Dmaven.wagon.http.ssl.insecure=true -Dmaven.wagon.http.ssl.allowall=true -Dmaven.wagon.http.ssl.ignore.validity.dates=true
