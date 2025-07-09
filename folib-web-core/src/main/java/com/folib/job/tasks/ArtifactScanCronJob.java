/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.job.tasks;

import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.scanner.service.SbomScannerService;
import jakarta.annotation.Resource;

/**
 * @author veadan
 **/
public class ArtifactScanCronJob extends JavaCronJob {

    @Resource
    private SbomScannerService scanService;

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
