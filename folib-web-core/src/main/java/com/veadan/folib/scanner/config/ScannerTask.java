package com.veadan.folib.scanner.config;


import cn.hutool.core.date.DateUtil;
import com.veadan.folib.scanner.biz.FolibScannerBiz;
import com.veadan.folib.scanner.entity.FolibScanner;
import com.veadan.folib.scanner.service.ScanService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.List;

@Slf4j
@Component
@EnableScheduling
public class ScannerTask {
    @Autowired
    private FolibScannerBiz folibScannerBiz;
    @Autowired
    private ScanService scanService;

    @Autowired
    private ScanConfig scanConfig;

    @Scheduled(cron = "0 0/5 * * * ? ")
    public void run() {
        //将正在扫描中的变为失败
        folibScannerBiz.updateScaning();
        List<FolibScanner> folibScanners = folibScannerBiz.selectEnableScan();
        folibScanners.forEach(folibScanner -> scanService.asyncScan(folibScanner));
        log.info("=====>>>>>当前线程名称：{}，使用cron异步执行：{}", Thread.currentThread().getName(), DateUtil.now());
    }
}
