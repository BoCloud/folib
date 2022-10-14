package com.veadan.folib.scanner.config;


import com.veadan.folib.scanner.biz.FolibScannerBiz;
import com.veadan.folib.scanner.entity.FolibScanner;
import com.veadan.folib.scanner.service.ScanService;
import com.veadan.folib.services.FolibDistributedSchedulerLock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@EnableScheduling
public class ScannerTask {
    private static final Logger logger = LoggerFactory.getLogger(
            ScannerTask.class);

    @Autowired
    private FolibScannerBiz folibScannerBiz;
    @Autowired
    private ScanService scanService;

    @Autowired
    private ScanConfig scanConfig;

    @Autowired
    private FolibDistributedSchedulerLock folibDistributedSchedulerLock;

    @Scheduled(cron = "0 0/5 * * * ? ")
    public void run() {
        logger.info("Wait for the lock [folib.ScannerTask]");
        if(folibDistributedSchedulerLock.getLock("folib.ScannerTask",300L)){
            logger.info("Get lock [folib.ScannerTask]");
            //FileUtil.del(new File(scanConfig.getScanDir()));

//        FolibScanner query=new FolibScanner().setScanStatus(ScanConstans.UNSCAN).setOnScan(true);
            //将正在扫描中的变为失败
            folibScannerBiz.updateScaning();

            List<FolibScanner> folibScanners = folibScannerBiz.selectEnableScan();
            folibScanners.forEach(folibScanner -> scanService.asyncScan(folibScanner));

//        Thread.sleep(6000);
            logger.info(Thread.currentThread().getName()+"=====>>>>>使用cron异步执行  {}"+(System.currentTimeMillis()/1000));
        }
        logger.info("ScannerTask end");

    }
}
