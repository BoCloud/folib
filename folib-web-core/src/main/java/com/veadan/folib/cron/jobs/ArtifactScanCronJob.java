package com.veadan.folib.cron.jobs;

import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.scanner.service.ScanService;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * @author leipenghui
 **/
public class ArtifactScanCronJob extends JavaCronJob {

    @Autowired
    private ScanService scanService;

    @Override
    protected void executeTask(CronTaskConfigurationDto config) throws Throwable {
        scanService.artifactScan("Cron Job");
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(ArtifactScanCronJob.class.getName())
                .name("定时扫描制品的任务")
                .description("定时扫描制品的任务")
                .build();
    }
}
