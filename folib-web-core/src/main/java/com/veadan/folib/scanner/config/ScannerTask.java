package com.veadan.folib.scanner.config;


import com.veadan.folib.scanner.biz.FolibScannerBiz;
import com.veadan.folib.scanner.entity.FolibScanner;
import com.veadan.folib.scanner.service.ScanService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.List;

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

           //FileUtil.del(new File(scanConfig.getScanDir()));

//        FolibScanner query=new FolibScanner().setScanStatus(ScanConstans.UNSCAN).setOnScan(true);
        //将正在扫描中的变为失败
        folibScannerBiz.updateScaning();

        List<FolibScanner> folibScanners = folibScannerBiz.selectEnableScan();
        folibScanners.forEach(folibScanner -> scanService.asyncScan(folibScanner));

//        Thread.sleep(6000);
        System.out.println(Thread.currentThread().getName()+"=====>>>>>使用cron异步执行  {}"+(System.currentTimeMillis()/1000));
    }
}
