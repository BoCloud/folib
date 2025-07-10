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
package com.folib.providers.layout.p2;

import com.folib.artifact.coordinates.P2Coordinates;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

public class P2ArtifactRuleProcessor
{

    private static final String KEY_START = "${";

    private static final String KEY_END = "}";

    private final String outputFormat;

    private final Map<String, String> properties = new HashMap<>();

    public P2ArtifactRuleProcessor(String outputFormat,
                                   String filter)
    {
        this.outputFormat = outputFormat;
        parseFilter(filter);
    }

    private void parseFilter(String filter)
    {
        String clearedFilter = filter.replaceAll("\\(", "");
        clearedFilter = clearedFilter.replaceAll("\\)", "");

        String[] splittedFilter = clearedFilter.split(" ");
        for (int i = 0; i < splittedFilter.length; i++)
        {
            String value = splittedFilter[i];
            if (i != 0)
            {
                String[] keyValuePair = value.split("=");
                properties.put(keyValuePair[0], keyValuePair[1]);
            }
        }
    }

    public boolean matches(Map<String, String> properties)
    {
        if (properties == null || properties.isEmpty())
        {
            return false;
        }

        boolean result = false;
        for (Entry<String, String> entry : this.properties.entrySet())
        {
            final String key = entry.getKey();
            if (properties.containsKey(key))
            {
                if (result)
                {
                    result &= this.properties.get(key).equals(properties.get(key));
                }
                else
                {
                    result = this.properties.get(key).equals(properties.get(key));
                }
            }
            else
            {
                if (result)
                {
                    result = false;
                }
            }
        }

        return result;
    }

    public String getOutput(P2Coordinates p2artifact)
    {
        Map<String, String> properties = p2artifact.getProperties();
        if (matches(properties))
        {
            String output = outputFormat;
            for (String key : getOutputPutKeys())
            {
                output = replaceValue(output, key, properties.get(key));
            }

            return output;
        }

        return null;
    }

    private String replaceValue(String format,
                                String key,
                                String value)
    {
        return format.replace(KEY_START + key + KEY_END, value);
    }

    private Collection<String> getOutputPutKeys()
    {
        final Collection keys = new ArrayList();

        final int startOffset = KEY_START.length();
        final int endOffset = KEY_END.length();

        String format = outputFormat;
        int startIndex = 0;
        while (startIndex >= 0)
        {
            String key = format.substring(startIndex + startOffset, format.indexOf(KEY_END));
            format = format.substring(format.indexOf(KEY_END) + endOffset);
            keys.add(key);
            startIndex = format.indexOf(KEY_START);
        }

        return keys;
    }

    public static String getFilename(P2Mappings mappings,
                                     P2Coordinates p2artifact)
    {
        Collection<P2ArtifactRuleProcessor> artifacts = mappings.getRules().stream().map(
                rule -> new P2ArtifactRuleProcessor(rule.getOutput(), rule.getFilter())).collect(
                Collectors.toList());

        for (P2ArtifactRuleProcessor processor : artifacts)
        {
            if (processor.matches(p2artifact.getProperties()))
            {
                return processor.getOutput(p2artifact);
            }
        }

        return null;
    }

}
