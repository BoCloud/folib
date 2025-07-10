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
package com.folib.dependency.snippet;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Component
public class SnippetGenerator {

    @Inject
    private CompatibleDependencyFormatRegistry compatibleDependencyFormatRegistry;

    public List<CodeSnippet> generateSnippets(String layout,
                                              ArtifactCoordinates coordinates) {
        Map<String, DependencySynonymFormatter> implementations = compatibleDependencyFormatRegistry.getProviderImplementations(layout);
        List<CodeSnippet> snippets = new ArrayList<>();
        if (Objects.nonNull(implementations)) {
            // Get the snippet for the default provider and make sure it's first in the list
            DependencySynonymFormatter defaultFormatter = implementations.get(layout);
            CodeSnippet defaultSnippet = new CodeSnippet(layout,
                    defaultFormatter.getDependencySnippet(coordinates));
            snippets.add(defaultSnippet);

            // Add the rest of the synonyms
            for (String compatibleDependencyFormat : implementations.keySet()) {
                if (compatibleDependencyFormat.equals(layout)) {
                    // We've already added this, before this loop.
                    continue;
                }

                DependencySynonymFormatter formatter = implementations.get(compatibleDependencyFormat);

                CodeSnippet snippet = new CodeSnippet(compatibleDependencyFormat, formatter.getDependencySnippet(coordinates));
                snippets.add(snippet);
            }
        }
        return snippets;
    }

}
