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
package com.folib.job.cron.jobs;

import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.fields.CronJobField;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Paths;
import java.util.Set;

import com.google.common.collect.ImmutableSet;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import groovy.lang.GroovyClassLoader;
import groovy.lang.GroovyCodeSource;

/**
 * @author Veadan
 * @author Yougeshwar
 */
public class GroovyCronJob
        extends AbstractCronJob
{

    private static final String PATH_PROPERTY = "script.path";

    // We do not require PATH_PROPERTY in the cron job definition.
    // The path is automatically set when the script is uploaded.
    private static final Set<CronJobField> FIELDS = ImmutableSet.of();

    @Override
    @SuppressFBWarnings(value = "DP_CREATE_CLASSLOADER_INSIDE_DO_PRIVILEGED")
    public void executeTask(CronTaskConfigurationDto config)
    {
        try
        {
            Class scriptClass = new GroovyClassLoader().parseClass(
                    new GroovyCodeSource(Paths.get(getScriptPath(config)).toUri()));
            Object scriptInstance = scriptClass.getConstructor().newInstance();
            //noinspection unchecked
            scriptClass.getDeclaredMethod("execute", new Class[]{}).invoke(scriptInstance);
        }
        catch (IOException | IllegalAccessException | InstantiationException | NoSuchMethodException | InvocationTargetException e)
        {
            logger.error("IOException: ", e);
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(GroovyCronJob.class.getName())
                                .name("Groovy Cron Job").scope("SYSTEM")
                                .fields(FIELDS)
                                .build();
    }

    public String getScriptPath(CronTaskConfigurationDto configuration)
    {
        return configuration.getRequiredProperty(PATH_PROPERTY);
    }

}
