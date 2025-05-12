# 多节点集群配置

## 前置说明

你需要准备至少8C16G的机器 **3台**，磁盘按照你的需求进行。

:::tip 注意
下方文章，我们将以3个节点，启动方式以docker启动为例，您也可以采用vm启动方式，目的主要用来进行集群节点的配置说明。

**此外，数据库mysql和es的集群模式不在本次教程范围内**
:::

## 集群同步配置

这里将以3个节点为例，命名为`node1`,`node2`,`node3`,分别在3台机器：`10.10.0.1`，`10.10.0.2`，`10.10.0.3`

- **STEP1:** node1 在为种子节点，启动脚本如下：

```shell
docker run -itd  --restart always --name folib-node1 -p 38080:38080 \
-p 7010:7010 -p 7011:7011 -p 7199:7199 -p 49142:49142 -p 8182:8182 \
-e FOLIB_MYSQL_HOST=mysql \
-e FOLIB_MYSQL_PORT=3306 \
-e FOLIB_MYSQL_DB=folib_scanner \
-e FOLIB_MYSQL_USER=root \
-e FOLIB_MYSQL_PASSWORD=folib@v587 \
-e FOLIB_ES_HOST=elasticsearch-server:9200 \
-e FOLIB_PORT=38080 \
-e FOLIB_NVD=folib-mirror \
-e FOLIB_CLUSTER_OPENFLAG=true \                   #集群模式开关true为开启，false为关闭。
-e FOLIB_DISTRIBUTED_LOCKIP=10.10.0.1 \            #指定当前节点的VIP或者IP，该地址用于向其他节点暴露当前节点IP使用。
-e FOLIB_CLUSTER_HOSTNODE=10.10.0.2:38080,10.10.0.3:38080        #配置上其它节点的host地址
-v /home/folib/folib-conf:/opt/folib/folib-1.0-SNAPSHOT/etc/conf \
-v /home/folib/folib-vault:/opt/folib/folib-vault  \
--link elasticsearch-server:elasticsearch-server \
--link mysql:mysql \
--link folib-mirror:folib-mirror \
58.210.154.140:2477/folib-common/folib-docker/folib-server:1.0
```

- **STEP2:** 修改 node1 的图数据库持久化配置文件如下：

①修改cassandra.yaml

```shell
$ cd /home/folib/foib-conf
$ vim cassandra.yaml
```

②编辑监听地址为 `node1` 的IP， `seed` 地址为 `node1` 节点 `IP` ，请参照下方 **注释的位置**

```yaml
## ---------上方省略具体以实际为准---------------

listen_address: 10.10.0.1        #当前主机IP
storage_port: 7010
ssl_storage_port: 7011
start_native_transport: true
native_transport_port: 49142
native_transport_max_threads: 256

read_request_timeout_in_ms: 5000
range_request_timeout_in_ms: 10000
write_request_timeout_in_ms: 2000
cas_contention_timeout_in_ms: 1000
truncate_request_timeout_in_ms: 60000
request_timeout_in_ms: 10000
cross_node_timeout: false

seed_provider:
    - class_name: org.apache.cassandra.locator.SimpleSeedProvider
      parameters:
          - seeds: "10.10.0.1"      #种子节点IP，node1为种子节点，其它节点均填写该IP
## --------下方省略----------------
```

- **STEP3:** 修改 node1 的图数据库配置文件如下：

①修改 `janusgraph-cassandra.properties`

```shell
$ cd /home/folib/foib-conf
$ vim janusgraph-cassandra.properties
```

②编辑 `node1` 的 `janusgragh` 的 `host` 地址为当前 `IP` ，请参照下方 **注释的位置**

```properties
## ---------上方省略具体以实际为准---------------
storage.backend = cql
storage.hostname = 10.10.0.1     #该hostname为当前主机IP
storage.port = 49142
storage.username = root
## --------下方省略----------------
```

- **STEP4:**  修改 node2 ， node3 的配置如下：

`node2` 配置如下：

```yaml
##cassandra.yaml
## ---------上方省略具体以实际为准---------------
listen_address: 10.10.0.2        #当前node2的IP
storage_port: 7010
ssl_storage_port: 7011
start_native_transport: true
native_transport_port: 49142
native_transport_max_threads: 256

read_request_timeout_in_ms: 5000
range_request_timeout_in_ms: 10000
write_request_timeout_in_ms: 2000
cas_contention_timeout_in_ms: 1000
truncate_request_timeout_in_ms: 60000
request_timeout_in_ms: 10000
cross_node_timeout: false

seed_provider:
    - class_name: org.apache.cassandra.locator.SimpleSeedProvider
      parameters:
          - seeds: "10.10.0.1"      #种子节点IP，node1为种子节点
## --------下方省略----------------
```

```properties
##janusgraph-cassandra.properties
## ---------上方省略具体以实际为准---------------
storage.backend = cql
storage.hostname = 10.10.0.2     #该hostname为当前主机IP
storage.port = 49142
storage.username = root
## --------下方省略----------------
```

`node3` 配置如下：

```yaml
##cassandra.yaml
## ---------上方省略具体以实际为准---------------
listen_address: 10.10.0.3        #当前node3的IP
storage_port: 7010
ssl_storage_port: 7011
start_native_transport: true
native_transport_port: 49142
native_transport_max_threads: 256

read_request_timeout_in_ms: 5000
range_request_timeout_in_ms: 10000
write_request_timeout_in_ms: 2000
cas_contention_timeout_in_ms: 1000
truncate_request_timeout_in_ms: 60000
request_timeout_in_ms: 10000
cross_node_timeout: false

seed_provider:
    - class_name: org.apache.cassandra.locator.SimpleSeedProvider
      parameters:
          - seeds: "10.10.0.1"      #种子节点IP，node1为种子节点
## --------下方省略----------------
```

```properties
##janusgraph-cassandra.properties
## ---------上方省略具体以实际为准---------------
storage.backend = cql
storage.hostname = 10.10.0.3     #该hostname为当前node3主机IP
storage.port = 49142
storage.username = root
## --------下方省略----------------
```

## 集群文件存储配置

- **方案一**  多节点共用S3对象存储

例如： `minio`, `aws`, `aliyun`, 腾讯云等支持 `S3` 协议的对象存储工具。

```bash
#如果采用S3协议的存储，可以配置S3，默认是采用本地NFS
FOLIB_S3_REGION="${FOLIB_S3_REGION:-folib}"
FOLIB_S3_URI="${FOLIB_S3_URI:-s3://localhost:9000/}"
FOLIB_S3_ACCESS_KEY="${FOLIB_S3_ACCESS_KEY:-folib}"
FOLIB_S3_SECRET_KEY="${FOLIB_S3_SECRET_KEY:-folib}"
```

- **方案二** 多节点间本地文件目录共享NFS

```shell
#node1机器上执行
yum -y install nfs-utils rpcbind
systemctl enable rpcbind
systemctl enable nfs-server
systemctl enable nfs-lock
systemctl enable nfs-idmap

systemctl start rpcbind
systemctl start nfs-server
systemctl start nfs-lock
systemctl start nfs-idmap

chmod -R 777 /home/folib/folib-vault

##其它节点
yum -y install nfs-utils
showmount -e 10.10.0.1

mount -t nfs 10.10.0.1:/home/folib/folib-vault /home/folib/folib-vault

##维护工具操作(当需要移除或重启的时候使用)
umount /home/folib/folib-vault
##重启nfs 服务
systemctl restart  nfs-server
```

:::tip 提示
也可以采用其他方式，以上举例为共享nfs方式。
:::