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
package com.folib.job.cron.services.support;

import java.util.HashMap;
import java.util.Map;

import org.springframework.util.CollectionUtils;

/**
 * @author veadan
 */
public class CronTaskConfigurationSearchCriteria
{

    private Map<String, String> properties;

    public boolean isEmpty()
    {
        return CollectionUtils.isEmpty(properties);
    }

    public Map<String, String> getProperties()
    {
        return properties;
    }

    public static final class CronTaskConfigurationSearchCriteriaBuilder
    {

        private Map<String, String> properties;

        private CronTaskConfigurationSearchCriteriaBuilder()
        {
        }

        public static CronTaskConfigurationSearchCriteriaBuilder aCronTaskConfigurationSearchCriteria()
        {
            return new CronTaskConfigurationSearchCriteriaBuilder();
        }

        public CronTaskConfigurationSearchCriteriaBuilder withProperties(Map<String, String> properties)
        {
            this.properties = properties;
            return this;
        }

        public CronTaskConfigurationSearchCriteriaBuilder withProperty(String key,
                                                                       String value)
        {
            if (properties == null)
            {
                properties = new HashMap<>();
            }
            this.properties.put(key, value);
            return this;
        }

        public CronTaskConfigurationSearchCriteria build()
        {
            CronTaskConfigurationSearchCriteria cronTaskConfigurationSearchCriteria = new CronTaskConfigurationSearchCriteria();
            cronTaskConfigurationSearchCriteria.properties = this.properties;
            return cronTaskConfigurationSearchCriteria;
        }
    }
}
