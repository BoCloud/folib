package com.veadan.folib.cron.jobs;

import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.task.AlarmNoticeTask;
import javax.inject.Inject;

public class AlarmNoticeCronJob extends JavaCronJob {

    @Inject
    private AlarmNoticeTask alarmNoticeTask;

    @Override
    protected void executeTask(CronTaskConfigurationDto config) throws Throwable {
        // 实现任务逻辑
        alarmNoticeTask.someMethod();
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(AlarmNoticeCronJob.class.getName())
                .name("定时发送告警通知的任务")
                .description("定时发送告警通知的任务")
                .build();
    }
}
