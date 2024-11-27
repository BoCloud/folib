#!/bin/bash
set -ex

IMAGE_NAME="172.30.193.101:38080/folib-common/docker-repo/folib-server"                # 你的镜像名称
TAG=$(date +%Y%m%d-%H%M)

SECONDS=0
script_dir=$(realpath $(dirname "$0"))
# folib-server project root dir
folib_root_dir=$(dirname "$script_dir")

# 编译容器
docker run -it --rm --name folib-ci-container -v yarnCache:/usr/local/share/.cache/yarn/v6 -v mavenm2:/root/.m2 -v $folib_root_dir:/usr/src/folib-server -w /usr/src/folib-server 58.210.154.140:2477/folib-common/folib-docker/folib-build-env:v1 sh folib-package.sh

unzip folib-distribution/target/folib-distribution-1.0-SNAPSHOT.zip

docker build --no-cache -t $IMAGE_NAME:$TAG folib-distribution-1.0-SNAPSHOT

docker push $IMAGE_NAME:$TAG

echo "Total build time: $SECONDS seconds."