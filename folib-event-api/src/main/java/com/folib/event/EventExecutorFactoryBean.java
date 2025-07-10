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
package com.folib.event;

import java.util.Optional;
import java.util.concurrent.Executor;


import cn.hutool.extra.spring.SpringUtil;
import jakarta.servlet.ServletContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.core.task.SyncTaskExecutor;

public class EventExecutorFactoryBean implements FactoryBean<Executor>
{

    private static final Logger logger = LoggerFactory.getLogger(EventExecutorFactoryBean.class);

    private final ServletContext servletContext;

    public EventExecutorFactoryBean(ServletContext servletContext)
    {
        super();
        this.servletContext = servletContext;
    }

    @Override
    public Executor getObject()
        throws Exception
    {
        Executor executor = Optional.ofNullable(servletContext)
                                    .flatMap(c -> Optional.ofNullable(lookupExecutor()))
                                    .orElse(new SyncTaskExecutor());
        
        logger.info("Using [{}] executor for Async events.", executor.getClass());
        
        return executor;
    }

    private Executor lookupExecutor()
    {
        Executor result;
        if ((result = lookupJettyExecutor()) != null)
        {
            return result;
        }
        return null;
    }

    private Executor lookupJettyExecutor()
    {
        Executor executor = (Executor) SpringUtil.getBean("asyncEventListenerExecutor");
        if (executor == null)
        {
            return null;
        }

        logger.info("Jetty environment detected.");

        return executor;
    }

    @Override
    public Class<?> getObjectType()
    {
        return Executor.class;
    }

}
