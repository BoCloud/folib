
## Resources
- Documentation is [here](https://demos.creative-tim.com/muse-vue-ant-design-dashboard/documentation?ref=readme-mvaddp)
- Issues: [Github Issues Page](https://github.com/creativetimofficial/muse-vue-ant-design-dashboard/issues)

- node版本 v14.19.1

## install
```shell
yarn install
yarn run serve
```
2. Providing us reproducible steps for the issue will shorten the time it takes for it to be fixed.
3. Some issues may be browser specific, so specifying in what browser you encountered the issue might help.

```shell
tar -cvf dist.tar dist
docker build -t folib/folib-web .
docker stop folib-web
docker rm folib-web
docker run -ti -d --restart=always   -p 9527:9527   --name folib-web folib/folib-web:latest

docker run -ti -d --restart=always   -p 9527:9527 -v/opt/folib/folib-web/:/etc/nginx/conf.d/  --name folib-web folib/folib-web:latest

```
