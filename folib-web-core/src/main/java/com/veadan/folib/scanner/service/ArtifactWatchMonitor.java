//package com.veadan.folib.scanner.service;
//
//import cn.hutool.core.io.watch.WatchMonitor;
//import cn.hutool.core.io.watch.Watcher;
//import cn.hutool.core.lang.Console;
//
//import com.veadan.folib.scanner.common.constant.ScanConstans;
//import com.veadan.folib.scanner.config.ScanConfig;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.CommandLineRunner;
//import org.springframework.stereotype.Component;
//
//import java.nio.file.Path;
//import java.nio.file.WatchEvent;
//
//@Component
//public class ArtifactWatchMonitor implements CommandLineRunner {
//    @Autowired
//    private ScanConfig scanConfig;
//
//    @Autowired
//    public ScanService scanService;
//
//    @Override
//    public void run(String... args){
//        //启动时开启
//        Console.log("开启对：{}的文件变化监听", scanConfig.getWatchMonitorPath()+"storages/");
//
//        WatchMonitor watchMonitor = WatchMonitor.create(scanConfig.getWatchMonitorPath()+"storages/", WatchMonitor.EVENTS_ALL);
//        watchMonitor.setWatcher(new Watcher(){
//            @Override
//            public void onCreate(WatchEvent<?> event, Path currentPath) {
//                Object obj = event.context();
//              boolean b=  scanService.checkScan(event,currentPath, ScanConstans.ADD);
//                Console.log("创建：{}/{}-> {}", currentPath,obj, b);
//            }
//
//            @Override
//            public void onModify(WatchEvent<?> event, Path currentPath) {
//                Object obj = event.context();
//                boolean b=  scanService.checkScan(event,currentPath, ScanConstans.UPDATE);
//                Console.log("更新：{}/{}-> {}", currentPath,obj, b);
//            }
//
//            @Override
//            public void onDelete(WatchEvent<?> event, Path currentPath) {
//                Object obj = event.context();
//                boolean b=  scanService.checkScan(event,currentPath, ScanConstans.DEL);
//                Console.log("删除：{}/{}-> {}", currentPath,obj, b);
//            }
//
//            @Override
//            public void onOverflow(WatchEvent<?> event, Path currentPath) {
//                Object obj = event.context();
//                boolean b=  scanService.checkScan(event,currentPath, ScanConstans.OVERFLOW);
//                Console.log("覆盖：{}/{}-> {}", currentPath,obj, b);
//            }
//        });
//
//        //设置监听目录的最大深入，目录层级大于制定层级的变更将不被监听，默认只监听当前层级目录
//        watchMonitor.setMaxDepth(30);
//        //启动监听
//        watchMonitor.start();
//    }
//}
