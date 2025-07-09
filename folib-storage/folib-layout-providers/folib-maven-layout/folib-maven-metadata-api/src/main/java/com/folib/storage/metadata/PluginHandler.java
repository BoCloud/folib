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
package com.folib.storage.metadata;

import java.util.HashMap;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Created by dinesh on 1/4/18.
 */
public class PluginHandler
        extends DefaultHandler
{

    boolean goalPrefix;
    boolean pluginName;

    String strGoalPrefix, strPluginName;
    HashMap<String, String> pluginMap;

    public HashMap<String, String> getPluginMap()
    {
        return pluginMap;
    }

    @Override
    public void startElement(String uri,
                             String localName,
                             String qName,
                             Attributes attributes)
            throws SAXException
    {

        if (qName.equalsIgnoreCase("plugin"))
        {
            pluginMap = new HashMap<>();
        }
        else if (!goalPrefix && qName.equalsIgnoreCase("goalPrefix"))
        {
            goalPrefix = true;
        }
        else if (!pluginName && qName.equalsIgnoreCase("name"))
        {
            pluginName = true;
        }

    }

    @Override
    public void endElement(String uri,
                           String localName,
                           String qName)
            throws SAXException
    {
        if (qName.equalsIgnoreCase("plugin"))
        {
            pluginMap.put("goalPrefix", strGoalPrefix);
            pluginMap.put("name", strPluginName);

        }
    }

    @Override
    public void characters(char ch[],
                           int start,
                           int length)
            throws SAXException
    {
        if (goalPrefix && null == strGoalPrefix)
        {
            strGoalPrefix = new String(ch, start, length);
        }
        else if (pluginName && null == strPluginName)
        {
            strPluginName = new String(ch, start, length);
        }
    }
}
