# admin-antd-vue-docs 这是用来做首页说明文档使用的
开发调试：
```shell
yarn install
yarn run docs:dev
```
打包步骤：
```shell

yarn run docs:build   #编译打包，将会在.vuepress文件夹下存在dist文件

cd doc/.vuepress   #
tar -cvf dist.tar dist

docker build -t 58.210.154.140:2477/folib-common/folib-docker/folib-doc:1.0 .
docker build -t folib/folib-doc .
docker run -ti -d --restart=always   -p 9529:9529  --name folib-doc 58.210.154.140:2477/folib-common/folib-docker/folib-doc:1.1
```