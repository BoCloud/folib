package com.veadan.folib.job.tasks;

import com.veadan.folib.job.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.job.cron.jobs.CronJobDefinition;
import com.veadan.folib.job.cron.jobs.JavaCronJob;
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
