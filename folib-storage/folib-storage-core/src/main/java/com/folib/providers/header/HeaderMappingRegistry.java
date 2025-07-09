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
package com.folib.providers.header;

import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
@Component
public class HeaderMappingRegistry
{

    public static final String USER_AGENT_UNKNOWN = "unknown/*";

    private static final String USER_AGENT_FORMAT = "%s/*";

    private Map<String, List<String>> layout2UserAgentKeywodMap = new LinkedHashMap<>();

    public HeaderMappingRegistry()
    {
    }

    public void register(String layoutProviderAlias,
                         String... userAgentKeywords)
    {
        layout2UserAgentKeywodMap.put(layoutProviderAlias,
                                      Arrays.stream(userAgentKeywords)
                                            .collect(Collectors.toList()));
    }

    public Optional<String> lookupUserAgent(String originalUserAgentHeaverValue)
    {
        if (originalUserAgentHeaverValue == null) {
            return Optional.empty();
        }
        
        return layout2UserAgentKeywodMap.values()
                                        .stream()
                                        .flatMap(l -> l.stream())
                                        .filter(s -> originalUserAgentHeaverValue.toUpperCase().contains(s.toUpperCase()))
                                        .map(this::formatHeader)
                                        .findFirst();
    }

    public String defaultLayoutUserAgent(String layout)
    {
        return Optional.ofNullable(layout2UserAgentKeywodMap.get(layout))
                       .flatMap(s -> s.stream().findFirst())
                       .map(this::formatHeader)
                       .orElse(USER_AGENT_UNKNOWN);
    }

    private String formatHeader(String s)
    {
        return String.format(USER_AGENT_FORMAT, s);
    }

}
