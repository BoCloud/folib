# 虚拟机安装

## 机器准备

你需要准备一个至少1台机器配置如下：

***CPU内存：*** 最小8C16G(X86或者ARM架构)。

***操作系统：*** CentOS,银河麒麟,统信UOS等操作系统

***磁盘空间：*** 最小50GB，通常情况下，1年 1TB的规模左右的增量速率。

## 数据库准备

- 第一步 你需要自己准备一个Mysql、MariaDB类型的数据库(版本5.7+)，并创建好数据库和用户名密码。
- 第二步 你需要自己准备一个elasticsearch(版本7.0+)，主要用来给图数据库提供元数据全文搜索使用；

:::tip 特别说明
无需初始化SQL，我们在应用第一次启动的时候会创建数据库和初始化数据。

folib **支持信创国产化服务器** ，数据库如有同类型的替代产品也可支持，例如：TiDB,OceanDB, 达梦等
:::

## 后端Linux安装

下载安装包（zip包）到具体目录

```bash
$ unzip folib.zip
```

## 解压后的目录结构说明

安装完成后的目录结构如下

```
├── folib-1.0                                    
│   ├── bin                                  
│   │   └── folib                                #进程启动命令工具
│   ├── etc                                      #配置主目录
│   │   ├── conf                                
│   │   │   ├── cassandra.yaml
|   |   |   ├── db_EMBEDDED.yaml
|   |   |   ├── db_MEMORY.yaml
|   |   |   ├── db_REMOTE.yaml
|   |   |   ├── folib-authentication-providers.yaml
|   |   |   ├── folib-authorization.yaml
|   |   |   ├── folib-cron-tasks.yaml
|   |   |   ├── folib-security-users.yaml
|   |   |   ├── folib.yaml
|   |   |   ├── janusgraph-cassandra.properties
|   |   |   └── janusgraph-inmemory.properties
│   ├── lib                                      
│   │   ├── folib-core-1.0-spring-boot.jar       #folib主进程启动包                                   
│   └── tmp                                      #临时文件路径
└── folib-vault                                  #数据与日志路径
```

:::tip
只有应用启动之后才会自动创建folib-vault数据文件路径
:::