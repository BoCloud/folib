package com.folib.task;


import com.folib.scanner.service.SbomScannerService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * 扫描task
 */
@Slf4j
@Component
@EnableScheduling
public class ScannerTask {

    @Autowired
    private SbomScannerService sbomScannerService;

    /**
     * 每5分钟
     */
    @Scheduled(cron = "0 0/5 * * * ? ")
    public void run() {
        sbomScannerService.artifactsScan();
    }
}
