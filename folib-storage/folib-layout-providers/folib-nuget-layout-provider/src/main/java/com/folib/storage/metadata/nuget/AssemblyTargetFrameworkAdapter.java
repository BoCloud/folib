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


package com.folib.storage.metadata.nuget;

import java.util.EnumSet;

import com.google.common.base.Joiner;
import com.google.common.base.Strings;
import jakarta.xml.bind.annotation.adapters.XmlAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * List to frameworks converter
 */
public class AssemblyTargetFrameworkAdapter extends XmlAdapter<String, EnumSet<Framework>>
{

    /**
     * Logger
     */
    private final static Logger logger = LoggerFactory.getLogger(AssemblyTargetFrameworkAdapter.class);

    /**
     * Frameworks delimeter
     */
    private static final String FRAMEWORKS_DELIMETER = ", ";

    @Override
    public String marshal(EnumSet<Framework> frameworks)
        throws Exception
    {
        if (frameworks == null || frameworks.isEmpty())
        {
            return null;
        }

        String result = Joiner.on(FRAMEWORKS_DELIMETER).join(frameworks);
        return result;
    }

    @Override
    public EnumSet<Framework> unmarshal(String farmeworks)
        throws Exception
    {
        if (Strings.isNullOrEmpty(farmeworks))
        {
            return null;
        }

        String[] names = farmeworks.split(FRAMEWORKS_DELIMETER);
        EnumSet<Framework> result = EnumSet.noneOf(Framework.class);
        for (String name : names)
        {
            try
            {
                final Framework framework = Framework.getByFullName(name);

                if (framework != null)
                {
                    result.add(framework);
                }
            }
            catch (Exception e)
            {
                logger.warn("Can not add framework: \"{}\"", name, e);
            }
        }

        if (result.isEmpty())
        {
            return null;
        }

        return result;
    }
}
