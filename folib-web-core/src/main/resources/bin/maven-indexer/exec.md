
### 命令架构路径

bin
├── darwin
│   ├── folib_index_reader_amd64
│   └── folib_index_reader_arm64
├── linux
│   └── folib_index_reader_amd64
└── windows
└── folib_index_reader.exe


### 参数执行
```shell
bin/darwin/folib_index_reader_amd64 --format json  --indexId maven-local --chainId 1692336180150 \
 --url http://10.10.33.149:8081/artifactory/maven-local/ > index.dump
```



### 参数说明

- -url string
仓库的url地址,地址最后不要加斜线(/)结尾，例如index source base URL
- -indexId string
必填参数:nexus-maven-repository-index.properties中的 index ID (default "maven-local")
- -format string
输出格式: one of 'log', 'json', 'csv' (default "log")
- -chainId string
nexus-maven-repository-index.properties 中的 chain ID (default "1692336180150")


###  如何寻nexus-maven-repository-index.properties？
http://10.10.33.149:8081/artifactory/maven-local-1/.index/nexus-maven-repository-index.properties
每个索引文件下都有这个文件
