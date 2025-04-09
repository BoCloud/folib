# VM启动命令详解

启动模式与命令详解（你可以选择下面的任意模式）

```bash
$ sh ./folib-1.0/bin/folib  start        #后台启动方式
$ sh ./folib-1.0/bin/folib  console      #启动并打开日志控制台，通常用于查看运行是否正常
$ sh ./folib-1.0/bin/folib  debug        #debug模式启动，通常用于远程debug来排查问题使用
$ sh ./folib-1.0/bin/folib  stop         #停止folib进程
$ sh ./folib-1.0/bin/folib  restart      #重启folib进程
$ sh ./folib-1.0/bin/folib  status       #查看进程状态
$ sh ./folib-1.0/bin/folib repair_start  # 只有集群模式下，且该节点发生宕机后进行使用。
```

:::tip
在启动之前，请确保数据库连接是正确的。
:::

:::warning 注意事项
在启动前请使用，netstat -tunlp 命令查看，38080，如果修改可以在folib中进行修改
:::