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


import jakarta.xml.bind.annotation.adapters.XmlAdapter;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author Unlocker
 */
public class StringListTypeAdapter extends XmlAdapter<String, List<String>>
{

    /**
     * Default splitter pattern
     */
    private String delimeter = "\\s+";

    /**
     * Remove whitespaces
     */
    private boolean trimSpaces = false;

    /**
     * Default constructor
     */
    public StringListTypeAdapter()
    {
    }

    /**
     * @param delimeter
     *            REGEXP delimiter
     * @param trimSpaces
     *            whether to trim spaces
     */
    public StringListTypeAdapter(String delimeter,
                                 boolean trimSpaces)
    {
        this.delimeter = delimeter;
        this.trimSpaces = trimSpaces;
    }

    @Override
    public List<String> unmarshal(String v)
    {
        String pattern = trimSpaces ? "\\s*" + delimeter + "\\s*" : delimeter;
        String[] temp = v.split(pattern);
        List<String> result = new ArrayList<>();

        for (String str : temp)
        {
            String tag = str.trim();
            if (!tag.isEmpty())
            {
                result.add(tag);
            }
        }

        return result;
    }

    @Override
    public String marshal(List<String> v)
        throws Exception
    {
        Iterator<String> iter = v.iterator();
        if (!iter.hasNext())
        {
            return "";
        }

        StringBuilder buffer = new StringBuilder(iter.next());
        while (iter.hasNext())
        {
            buffer.append(delimeter).append(iter.next());
        }

        return buffer.toString();
    }
}
