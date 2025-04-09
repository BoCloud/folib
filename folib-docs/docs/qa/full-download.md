# 同步支持

`Folib` 提供了 `pypi` 和 `npm` 两种同步方式

## PyPI - bandersnatch

+ **准备工作**

	+ 安装 *python3*

	```shell
    yum -y install zlib-devel bzip2-devel openssl-devel ncurses-devel sqlite-devel readline-devel tk-devel gdbm-devel db4-devel libpcap-devel xz-devel

    yum install libffi-devel -y

    wget https://www.python.org/ftp/python/3.7.3/Python-3.7.3.tar.xz

    tar -xvJf Python-3.7.3.tar.xz

    cd Python-3.7.3

    ./configure prefix=/usr/local/python3

    make && make install

    ln -s /usr/local/python3/bin/python3 /usr/bin/python3
    ```

    + 配置环境变量

    ```shell
    # >>> 设置环境变量 >>>
    vim ~/.bash_profile

    ## .bash_profile >>>
    # Get the aliases and functions
    if[ -f ~/.bashrc ]; then
    . ~/.bashrc
    fi

    # User specific environment and startup programs
    PATH=$PATH:$HOME/bin:/usr/local/python3/bin
    export PATH
    ## .bash_profile <<<
    # <<< 设置环境变量 <<<

    # >>> 刷新环境变量 >>>
    source ~/.bash_profile
    # <<< 刷新环境变量 <<<

    # >>> 测试验证环境 >>>
    python3 -V
	# example output: Python 3.7.3

	pip3 -V
    # example output: pip 19.0.3 from /usr/local/python3/lib/python3.7/site-packages/pip (python 3.7)
    # <<< 测试验证环境 <<<
    ```
+ **安装 bandersnatch**

	+ 安装

	```shell
    pip3 install bandersnatch

    find / -name master.py
    # example output: /usr/local/python3/lib/python3.7/site-packages/bandersnatch/master.py

    cp /usr/local/python3/lib/python3.7/site-packages/bandersnatch/master.py{,.bak}

    vim /usr/local/python3/lib/python3.7/site-packages/bandersnatch/master.py
    # example input: 12345
    ```

    + 生成配置

    ```shell
	bandersnatch mirror
 	# example output: WARNING: Config file '/etc/bandersnatch.conf' missing, creating default config.
 	# example output: WARNING: Please review the config file, then run 'bandersnatch' again.
	```

    + 修改配置文件 /etc//bandersnatch.conf，根据自己的情况修改

	```shell
    # vim /etc/bandersnatch.conf >>>
    [mirror]
    ; The directory where the mirror data will be stored.
    directory = /srv/pypi
    ; Save JSON metadata into the web tree:
    ; URL/pypi/PKG_NAME/json (Symlink) -> URL/json/PKG_NAME
    json = false

    ; The PyPI server which will be mirrored.
    ; master = https://pypi.python.org
    ; scheme for PyPI server MUST be https
    master = https://pypi.python.org
    timeout = 30
    # vim /etc/bandersnatch.conf <<<
    ```
	| 参数 | 参数阐释 |
	| :----: | :----: |
	| **directory pypi** | 本地存放位置 |
	| **master pypi** |	远程仓库地址 |
+ **下载包**

```shell
# 下载所有 package 至本地(更新本地的 pacakge也用这个命令)
bandersnatch -c /etc/bandersnatch.conf mirror
```

## npm - cnpmcore

+ **准备工作**

	> 如有现有的 MySQL redis minio 跳过步骤

    + *mysql* 安装

	```shell
    docker run -d \
      --name mysql \
      -v ~/mariadb/mysql_data:/var/lib/mysql \
      -e MYSQL_ROOT_PASSWORD="folib@v587" \
      -e TZ=Asia/Shanghai \
      -e MYSQL_INITDB_SKIP_TZINFO=yes \
      -e MYSQL_LOWER_CASE_TABLE_NAMES=1 \
      -e MAX_ALLOWED_PACKET=256M \
      -e INNODB_LOG_FILE_SIZE=1G \
      -e TZ=Asia/Shanghai \
      -e LANG=C.UTF-8 \
      -p 3306:3306 \
      mysql:8.0.32 \
      --innodb-large-prefix=ON \
      --innodb-file-format=Barracuda
    ```

    + *redis* 安装

	```shell
    docker run -itd --name redis \
    --restart=always \
    --log-opt max-size=100m \
    --log-opt max-file=2 \
    -p 6379:6379 \
    -v /data/redis/conf/redis.conf:/etc/redis/redis.conf \
    -v /data/redis/data:/data \
    redis redis-server /etc/redis/redis.conf --appendonly yes  --requirepass qwe123
    ```

    + *minio* 安装

	```shell
    docker run -d -p 9000:9000 --name minio\
      -e "MINIO_ACCESS_KEY=admin" \
      -e "MINIO_SECRET_KEY=admin123" \
      -v /data/npm-repo:/data
      minio/minio:RELEASE.2021-06-17T00-10-46Z server /data
    ```

    :::tip
	💡 minio 挂载/data 是存放npm 的位置
	:::

+ **制作镜像**

```shell
git clone https://github.com/cnpm/cnpmcore.git

# 修改配置
vim cnpmcore/config/config.default.ts

# 修改一下配置 >>>
// 同步源
sourceRegistry: 'https://registry.npmmirror.com',
sourceRegistryIsCNpm: true,

// changesStream
changesStreamRegistry: 'https://registry.npmmirror.com/_changes',
changesStreamRegistryMode: ChangesStreamMode.json,

enableChangesStream: true
syncMode: SyncMode.all,

docker build -t  cnpmcore:v1  .
# 修改一下配置 <<<
```

+ **安装**

```shell
docker run -p 7001:7001 -d \
  -e CNPMCORE_MYSQL_DATABASE=cnpmcore \
  -e CNPMCORE_MYSQL_HOST=172.17.0.2 \
  -e CNPMCORE_MYSQL_PORT=3306 \
  -e CNPMCORE_MYSQL_USER=root \
  -e CNPMCORE_MYSQL_PASSWORD=Mysql@123.! \
  -e CNPMCORE_NFS_TYPE=s3 \
  -e CNPMCORE_NFS_S3_CLIENT_ENDPOINT=http://172.17.0.5:9000 \
  -e CNPMCORE_NFS_S3_CLIENT_BUCKET=cpm \
  -e CNPMCORE_NFS_S3_CLIENT_ID=admin \
  -e CNPMCORE_NFS_S3_CLIENT_SECRET=admin123 \
  -e CNPMCORE_NFS_S3_CLIENT_FORCE_PATH_STYLE=true \
  -e CNPMCORE_NFS_S3_CLIENT_DISABLE_URL=true \
  -e CNPMCORE_REDIS_HOST=172.17.0.4 \
  -e CNPMCORE_REDIS_PORT=6379 \
  -e CNPMCORE_REDIS_DB=1 \
  -e TZ=Asia/Shanghai \
  --name cnpmcore cnpmcore:v1
```

+ **参数说明**

	+ *mysql*

	```shell
    CNPMCORE_MYSQL_DATABASE=cnpmcore #数据库名
    CNPMCORE_MYSQL_HOST=172.17.0.2 #数据库ip
    CNPMCORE_MYSQL_PORT=3306       #端口
    CNPMCORE_MYSQL_USER=your-db-user-name #用户
    CNPMCORE_MYSQL_PASSWORD=your-db-user-password #密码
    ```

    + *redis*

	```shell
    CNPMCORE_REDIS_HOST=172.17.0.4  #redis ip
    CNPMCORE_REDIS_PORT=6379        #redis 端口
    CNPMCORE_REDIS_PASSWORD=your-redis-password #密码 非必填
    CNPMCORE_REDIS_DB=1 #库
    ```

    + *minio*

	```shell
    CNPMCORE_NFS_TYPE=s3   #存储类型
    CNPMCORE_NFS_S3_CLIENT_ENDPOINT=http://172.17.0.5:9000 #访问地址
    CNPMCORE_NFS_S3_CLIENT_BUCKET=your-bucket-name #minio存储桶
    CNPMCORE_NFS_S3_CLIENT_ID=s3-ak   #用户名
    CNPMCORE_NFS_S3_CLIENT_SECRET=s3-sk #密码
    CNPMCORE_NFS_S3_CLIENT_DISABLE_URL=true
    ```