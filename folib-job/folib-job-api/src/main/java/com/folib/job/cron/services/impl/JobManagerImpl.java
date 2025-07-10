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
package com.folib.job.cron.services.impl;

import com.folib.job.cron.config.JobExecutionListener;
import com.folib.job.cron.services.JobManager;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class JobManagerImpl
        implements JobManager
{

    private final Map<String, JobExecutionListener> listenerRegistry;

    private final Map<String, Boolean> executedJobs;

    public JobManagerImpl()
    {
        listenerRegistry = new HashMap<>();
        executedJobs = new HashMap<>();
    }

    public synchronized void addExecutedJob(String jobName,
                                            Boolean statusExecuted)
    {
        executedJobs.put(jobName, statusExecuted);
        getJobExecutionListener(jobName).ifPresent(listener -> listener.onJobExecution(jobName, statusExecuted));
    }

    public Map<String, Boolean> getExecutedJobs()
    {
        return executedJobs;
    }

    @Override
    public void registerExecutionListener(String jobName,
                                          JobExecutionListener executionListener)
    {
        if (jobName == null)
        {
            throw new IllegalArgumentException("Cannot define a null value for jobName!");
        }
        if (executionListener == null)
        {
            throw new IllegalArgumentException("Cannot define a null value for executionListener");
        }

        listenerRegistry.put(jobName, executionListener);
    }

    @Override
    public Optional<JobExecutionListener> getJobExecutionListener(String jobName)
    {
        return Optional.ofNullable(listenerRegistry.get(jobName));
    }

}
