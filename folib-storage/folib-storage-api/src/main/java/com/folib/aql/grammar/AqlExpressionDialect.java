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
package com.folib.aql.grammar;

import java.util.Optional;

import com.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.folib.data.criteria.DefaultExpressionDialect;
import com.folib.data.criteria.Expression;
import com.folib.data.criteria.QueryParserException;
import org.springframework.util.Assert;

/**
 * This class used to provice AQL specific {@link Expression} parseing.
 * 
 * @author veadan
 *
 */
public class AqlExpressionDialect extends DefaultExpressionDialect
{

    /**
     * AQL keyword mapping
     */
    private AqlMapping keyword;

    /**
     * Expression value.
     */
    private String value;

    /**
     * Binary flags to determine parsing position in current expression.
     * We need to be sure that we move step by step correctly: parse expression
     * property (01) -> parse expression value (10) -> parse expression operator
     * (11)
     */
    private int state = 0;

    @Override
    public String parseValue(String value)
    {
        Assert.state(state == 1, "You should process property first.");

        state = state | 2;
        String result = super.parseValue(value);
        this.value = probeForWildcardValue(result).map(v -> v.replaceAll("\\*", "%"))
                                                  .orElse(result);
        if (AqlMapping.LAYOUT.equals(keyword))
        {
            this.value = ArtifactLayoutLocator.getLayoutEntityMap()
                                              .entrySet()
                                              .stream()
                                              .filter(e -> e.getKey()
                                                            .equals(this.value))
                                              .map(e -> e.getValue())
                                              .findFirst()
                                              .orElseThrow(() -> new QueryParserException(
                                                      String.format("Unknown layout [%s].",
                                                                    value)))
                                              .getArtifactCoordinatesClass()
                                              .getSimpleName();

        }

        return this.value;
    }

    @Override
    public String parseProperty(String attribute)
    {
        Assert.state(state == 0,
                     String.format("Dirty parse context, you should use new [%s] instance to start parse new expression.",
                                   AqlExpressionDialect.class.getSimpleName()));

        state = state | 1;
        for (AqlMapping aqlKeyword : AqlMapping.values())
        {
            if (!aqlKeyword.toString().equalsIgnoreCase(attribute))
            {
                continue;
            }
            this.keyword = aqlKeyword;
            return aqlKeyword.property();
        }
        return String.format("artifactCoordinates.coordinates.%s",
                             attribute);
    }

    @Override
    public Expression.ExpOperator parseOperator(String operator)
    {
        Assert.state(state == 3, "You should process property and value first.");

        if (AqlMapping.FROM.equals(keyword))
        {
            return Expression.ExpOperator.GE;
        }
        else if (AqlMapping.TO.equals(keyword))
        {
            return Expression.ExpOperator.LE;
        }
        else if (AqlMapping.AGE.equals(keyword))
        {
            return Expression.ExpOperator.LE;
        }
        else if (value != null && value.contains("%"))
        {
            return Expression.ExpOperator.LIKE;
        }
        return Expression.ExpOperator.EQ;
    }

    private Optional<String> probeForWildcardValue(String value)
    {
        return Optional.ofNullable(value)
                       .filter(v -> v.startsWith("*") || v.endsWith("*"));
    }

}
