#!/usr/bin/env bash
#set -ex

mvn clean package --settings folib-settings.xml -Dmaven.test.skip=true -Dmaven.wagon.http.ssl.insecure=true -Dmaven.wagon.http.ssl.allowall=true -Dmaven.wagon.http.ssl.ignore.validity.dates=true
