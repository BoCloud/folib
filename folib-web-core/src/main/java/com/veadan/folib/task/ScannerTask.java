package com.veadan.folib.task;


import com.veadan.folib.scanner.service.ScanService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * @author leipenghui
 * 扫描task
 */
@Slf4j
@Component
@EnableScheduling
public class ScannerTask {

    @Autowired
    private ScanService scanService;

    /**
     * 每5分钟
     */
    @Scheduled(cron = "0 0/1 * * * ? ")
    public void run() {
        scanService.artifactsScan();
    }
}
