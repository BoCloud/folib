# 配置参数详解

-  **环境变量配置** 如下表：

| 变量名 | 默认值 | 说明 |
| :----: | :----: | :----: |
| `FOLIB_HTTP_PORT` | （空） | HTTP 端口 |
| `FOLIB_SSL_ENABLED` | `false` | 是否启用 SSL |
| `FOLIB_SSL_KEY_STORE` | （空） | SSL 密钥存储路径 |
| `FOLIB_SSL_KEY_STORE_TYPE` | （空） | SSL 密钥存储类型 |
| `FOLIB_SSL_KEY_STORE_PASSWORD` | （空） | SSL 密钥存储密码 |
| `FOLIB_SSL_KEY_ALIAS` | （空） | SSL 密钥别名 |
| `FOLIB_SSL_KEY_PASSWORD` | （空） | SSL 密钥密码 |
| `FOLIB_SSL_TRUST_STORE` | （空） | SSL 信任存储路径 |
| `FOLIB_SSL_TRUST_STORE_PASSWORD` | （空） | SSL 信任存储密码 |
| `FOLIB_SSL_TRUST_STORE_TYPE` | （空） | SSL 信任存储类型 |
| `FOLIB_ARTIFACT_DOWNLOAD_IMMEDIATELY_UPDATE` | `false` | 是否立即更新下载的构件 |
| `FOLIB_DOCKER_BROWSE_COMPATIBILITY` | `false` | Docker 浏览兼容性 |
| `FOLIB_CLUSTER_OPENFLAG` | `false` | 是否开启集群 |
| `FOLIB_DISTRIBUTED_LOCKIP` | `127.0.0.1` | 当前机器内网 IP（并非 VIP），用于标识多实例集群中的当前机器 |
| `FOLIB_STORAGE_BASE_DIR` | `$FOLIB_DEFAULT_VAULT` | 存储基础目录（如果 NAS 存储地址不在 VAULT 下，请配置其他地址） |
| `FOLIB_HOME` | `$FOLIB_DEFAULT_HOME` | FOLIB 的主目录 |
| `FOLIB_VAULT` | `$FOLIB_DEFAULT_VAULT` | FOLIB 的存储目录 |
| `FOLIB_PID_FILE` | `$FOLIB_VAULT/folib.pid` | 进程 PID 文件路径 |
| `FOLIB_JVM_XMX` | `4096m` | JVM 最大堆内存 |
| `FOLIB_JVM_XMS` | `4096m` | JVM 初始堆内存 |
| `FOLIB_JVM_XSS` | `256k` | JVM 线程栈大小 |
| `FOLIB_JVM_PARALLEL_GC_THREADS` | `8` | JVM 并行 GC 线程数 |
| `FOLIB_JVM_MAX_DIRECT_MEMORY_SIZE` | `2048m` | JVM 最大直接内存 |
| `FOLIB_PORT` | `38080` | FOLIB 运行端口 |
| `FOLIB_DB_PROFILE` | `db_EMBEDDED` | 数据库配置模式 |
| `FOLIB_GREMLIN_SERVER_ENABLED` | `false` | 是否启用 Gremlin Server |
| `FOLIB_LOG_CONSOLE_ENABLED` | `false` | 是否启用控制台日志 |
| `FOLIB_LOG_FILE_ENABLED` | `true` | 是否启用文件日志 |
| `FOLIB_LOG_FILE_SIZE_SINGLE` | `128MB` | 单个日志文件最大大小 |
| `FOLIB_LOG_FILE_SIZE_TOTAL` | `1GB` | 总日志文件大小限制 |
| `FOLIB_LOG_FILE_HISTORY` | `31` | 日志文件保留天数 |
| `FOLIB_DEBUG` | `false` | 是否开启调试模式 |
| `FOLIB_NPM_REMOTE_CHANGES_ENABLED` | `false` | 是否允许 NPM 远程变更 |
| `FOLIB_NUGET_DOWNLOAD_FEED` | `false` | 是否启用 NuGet 下载源 |
| `FOLIB_DOWNLOAD_INDEXES` | `false` | 是否下载索引 |
| `FOLIB_REMOTE_DB_HOST` | `127.0.0.1` | 远程数据库主机地址 |
| `FOLIB_REMOTE_DB_PORT` | `49142` | 远程数据库端口 |
| `FOLIB_REMOTE_DB_USER` | `root` | 远程数据库用户名 |
| `FOLIB_REMOTE_DB_PASS` | `folib-cassandra` | 远程数据库密码 |
| `FOLIB_EMBEDDED_DB_HOST` | `127.0.0.1` | 内嵌数据库主机地址 |
| `FOLIB_CASSANDRA_LISTEN_ADDRESS` | `127.0.0.1` | Cassandra 监听地址 |
| `FOLIB_CASSANDRA_SEEDS` | `127.0.0.1` | Cassandra 种子节点 |
| `FOLIB_CLUSTER_NODE_TOTAL` | `3` | 集群节点总数 |
| `FOLIB_CASSANDRA_GC_GRACE_SECONDS` | `864000` | Cassandra GC 宽限时间（秒） |
| `FOLIB_JMX_PORT` | `7199` | JMX 监听端口 |
| `FOLIB_S3_REGION` | `folib` | S3 存储区域 |
| `FOLIB_S3_URI` | `s3://localhost:9000/` | S3 存储地址 |
| `FOLIB_S3_ACCESS_KEY` | `folib` | S3 访问密钥 |
| `FOLIB_S3_SECRET_KEY` | `folib` | S3 密钥 |
| `FOLIB_ES_HOST` | `127.0.0.1` | ES 服务器地址 |
| `FOLIB_PROMOTION_BLOCK` | `false` | 是否启用自动晋级阻断 |
| `FOLIB_MYSQL_HOST` | `127.0.0.1` | MySQL 数据库主机地址 |
| `FOLIB_MYSQL_PORT` | `3306` | MySQL 端口 |
| `FOLIB_MYSQL_DB` | `folib_scanner` | MySQL 数据库名称 |
| `FOLIB_MYSQL_USER` | `root` | MySQL 用户名 |
| `FOLIB_MYSQL_PASSWORD` | `199088926` | MySQL 密码 |
| `FOLIB_NVD` | `nvd.folib.com/feeds/json/cve/1.1` | NVD 漏洞数据源 |
| `FOLIB_REDIS_ENABLED` | `false` | 是否启用 Redis |
| `FOLIB_REDIS_HOST` | `127.0.0.1` | Redis 服务器地址 |
| `FOLIB_REDIS_PASSWORD` | `123456` | Redis 访问密码 |
| `FOLIB_WEB_URL_PREFIX` | `/ui/` | UI 访问前缀 |
| `FOLIB_CACHE_PORT` | `5701` | Hazelcast 缓存端口 |
| `FOLIB_CACHE_CLUSTER` | `127.0.0.1:5701` | Hazelcast 集群地址 |
| `FOLIB_SWAGGER_ENABLE` | `false` | 是否启用 Swagger |
| `FOLIB_THREAD_POOL_COMMON_CORE` | `12` | 通用线程池核心线程数 |
| `FOLIB_THREAD_POOL_COMMON_MAX` | `12` | 通用线程池最大线程数 |
| `FOLIB_THREAD_POOL_COMMON_QUEUE` | `100000000` | 通用线程池队列大小 |
| `FOLIB_THREAD_POOL_SCAN_CORE` | `2` | 扫描线程池核心线程数 |
| `FOLIB_THREAD_POOL_SCAN_MAX` | `2` | 扫描线程池最大线程数 |
| `FOLIB_THREAD_POOL_SCAN_QUEUE` | `12` | 扫描线程池队列大小 |
| `FOLIB_THREAD_POOL_EVENT_LOG_CORE` | `2` | 事件日志线程池核心线程数 |
| `FOLIB_THREAD_POOL_EVENT_LOG_MAX` | `2` | 事件日志线程池最大线程数 |
| `FOLIB_THREAD_POOL_EVENT_LOG_QUEUE` | `100` | 事件日志线程池队列大小 |
| `FOLIB_THIRDPARTY_FOEYES_ENABLE` | `false` | 是否启用 FoEyes 第三方服务 |
| `FOLIB_THIRDPARTY_FOEYES_BASEURL` | `http://127.0.0.1:9527` | FoEyes 基础 URL |
| `FOLIB_THIRDPARTY_FOEYES_ACCESS_KEY` | `racdvMVzV9Wnqu8NAfQkyrsD0a2N0fNE` | FoEyes 访问密钥 |
| `FOLIB_THREAD_POOL_WORKER` | `6` | 线程池工作线程数 |
| `FOLIB_GREMLIN_POOL` | `6` | Gremlin 连接池大小 |
| `FOLIB_MAX_WORK_QUEUE_SIZE` | `16384` | 最大工作队列大小 |
| `FOLIB_MAX_SESSION_TASK_QUEUE_SIZE` | `8192` | 最大会话任务队列大小 |
| `FOLIB_MAX_CONTENT_LENGTH` | `1048576` | 最大内容长度（字节） |
| `FOLIB_MAX_CHUNK_SIZE` | `65536` | 最大数据块大小（字节） |
| `FOLIB_MAX_ACCUMULATION_BUFFER_COMPONENTS` | `2048` | 最大累积缓冲区组件数 |
| `FOLIB_RESULT_ITERATION_BATCH_SIZE` | `256` | 结果迭代批处理大小 |
| `FOLIB_USE_EPOLL_EVENT_LOOP` | `true` | 是否使用 EPOLL 事件循环 |
| `FOLIB_IDLE_CONNECTION_TIMEOUT` | `30000` | 空闲连接超时时间（毫秒） |
| `FOLIB_EVALUATION_TIMEOUT` | `30000` | 评估超时时间（毫秒） |
| `FOLIB_WRITE_BUFFER_HIGH_WATER_MARK` | `65536` | 写缓冲区高水位标记（字节） |
| `FOLIB_WRITE_BUFFER_LOW_WATER_MARK` | `32768` | 写缓冲区低水位标记（字节） |
| `FOLIB_ARTIFACT_UPLOAD_RESTRICTIONS` | `false` | 是否启用 artifact 上传限制 |
| `FOLIB_HTTP_PORT` | （空） | HTTP 端口 |
| `FOLIB_SSL_ENABLED` | `false` | 是否启用 SSL |
| `FOLIB_SSL_KEY_STORE` | （空） | SSL 密钥库路径 |
| `FOLIB_SSL_KEY_STORE_TYPE` | （空） | SSL 密钥库类型 |
| `FOLIB_SSL_KEY_STORE_PASSWORD` | （空） | SSL 密钥库密码 |
| `FOLIB_SSL_KEY_ALIAS` | （空） | SSL 密钥别名 |
| `FOLIB_SSL_KEY_PASSWORD` | （空） | SSL 密钥密码 |
| `FOLIB_SSL_TRUST_STORE` | （空） | SSL 信任库路径 |
| `FOLIB_SSL_TRUST_STORE_PASSWORD` | （空） | SSL 信任库密码 |
| `FOLIB_SSL_TRUST_STORE_TYPE` | （空） | SSL 信任库类型 |
| `FOLIB_ARTIFACT_DOWNLOAD_IMMEDIATELY_UPDATE` | `false` | 是否立即更新下载的 artifact |
| `FOLIB_DOCKER_BROWSE_COMPATIBILITY` | `false` | 是否启用 Docker 浏览兼容模式 |

- **VM环境变量** 配置参数修改方式：

**SETP1** :你需要通过编辑工具例如：vi或vim 编辑folib-1.0/bin/folib

```shell
vim folib-1.0/bin/folib
```

**SETP2** :你将可以进行类似下图，进行编辑folib中配配置参数。

```shell
#!/usr/bin/env bash
##-----以上省略-----
#REMOT cassandra相关只有db_REMOTE时才生效
FOLIB_REMOTE_DB_HOST="${FOLIB_REMOTE_DB_HOST:-127.0.0.1}"
FOLIB_REMOTE_DB_PORT="${FOLIB_REMOTE_DB_PORT:-49142}"
FOLIB_REMOTE_DB_USER="${FOLIB_REMOTE_DB_USER:-root}"
FOLIB_REMOTE_DB_PASS="${FOLIB_REMOTE_DB_PASS:-folib-cassandra}"

#jmx相关
FOLIB_JMX_PORT="${FOLIB_JMX_PORT:-7199}"

#如果采用S3协议的存储，可以配置S3，默认是采用本地NFS
FOLIB_S3_REGION="${FOLIB_S3_REGION:-folib}"
FOLIB_S3_URI="${FOLIB_S3_URI:-s3://localhost:9000/}"
FOLIB_S3_ACCESS_KEY="${FOLIB_S3_ACCESS_KEY:-folib}"
FOLIB_S3_SECRET_KEY="${FOLIB_S3_SECRET_KEY:-folib}"

##图数据库索引ES节点配置
FOLIB_ES_HOST="${FOLIB_ES_HOST:-10.50.8.55}"

##scanner相关配置
FOLIB_MYSQL_HOST="${FOLIB_MYSQL_HOST:-10.10.0.1}"  #数据库地址
FOLIB_MYSQL_PORT="${FOLIB_MYSQL_PORT:-3306}"    #数据库端口
FOLIB_MYSQL_DB="${FOLIB_MYSQL_DB:-folib}"     #数据库名称
FOLIB_MYSQL_USER="${FOLIB_MYSQL_USER:-root}"   #用户名
FOLIB_MYSQL_PASSWORD="${FOLIB_MYSQL_PASSWORD:-password}"  #密码
FOLIB_NVD="${FOLIB_NVD:-nvd.folib.com}"    #如果内部部署漏洞镜像则修改

##-----以下省略------
```