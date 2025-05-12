# Docker 容器部署

## 安装脚本

- **MySQL安装**

(该镜像基于开源镜像，开启了group——by等配置操作)

```shell
docker run -d  -p 3306:3306 --name mysql \
-v /home/folib/mysql/logs:/var/log/mysql \
-v /home/folib/mysql/data:/var/lib/mysql \
--restart=always --privileged=true \
-e MYSQL_ROOT_PASSWORD=folib@v587 \
-d docker.folib.com/folib-common/folib-docker/mysql5.7:latest --lower_case_table_names=1
```

- **ES安装**

```shell
mkdir -p /home/folib/es/
chmod 777 /home/folib/es/
docker run -itd -p 9200:9200 -p 9300:9300 --restart=always --privileged=true --name elasticsearch-server \
-v /home/folib/es/data:/usr/share/elasticsearch/data \
-v /home/folib/es/plugins:/usr/share/elasticsearch/plugins \
-e "discovery.type=single-node" \
-e ES_JAVA_OPTS="-Xms512m -Xmx512m" \
docker.folib.com/folib-common/folib-docker/elasticsearch:7.3.1
```

- **漏洞服务mirror安装**

```shell
mkdir  /home/folib/mirror/
chmod 777 /home/folib/mirror/
docker run -d   --name folib-mirror   -p 9998:80  \
 --mount type=bind,source=/home/folib/mirror,target=/usr/local/apache2/htdocs \
  docker.folib.com/folib-common/folib-docker/folib-mirror:1.0
```

- **folib-server安装**

```shell
docker run -itd  --restart always --name folib -p 38080:38080 \
-p 7010:7010 -p 7011:7011 -p 7199:7199 -p 49142:49142 -p 8182:8182 \
-e FOLIB_MYSQL_HOST=mysql \
-e FOLIB_MYSQL_PORT=3306 \
-e FOLIB_MYSQL_DB=folib_scanner \
-e FOLIB_MYSQL_USER=root \
-e FOLIB_MYSQL_PASSWORD=folib@v587 \
-e FOLIB_ES_HOST=elasticsearch-server:9200 \
-e FOLIB_PORT=38080 \
-e FOLIB_NVD=folib-mirror \
-v /home/folib/folib-conf:/opt/folib/folib-1.0-SNAPSHOT/etc/conf \
-v /home/folib/folib-vault:/opt/folib/folib-vault  \
--link elasticsearch-server:elasticsearch-server \
--link mysql:mysql \
--link folib-mirror:folib-mirror \
docker.folib.com/folib-common/folib-docker/folib-server:1.0
```

:::warning 注意事项
环境变量可以参考上方 [**🔗 环境变量表**](../../docs/deploy/configration-deploy.md#配置参数详解) 进行参考配置
:::