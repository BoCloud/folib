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
package com.folib.nuget.filter;

import com.folib.data.criteria.QueryParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import com.folib.domain.ArtifactEntity;

/**
 * @author veadan
 *
 */
public class NugetODataFilterQueryParser extends QueryParser<NugetODataFilterParser.FilterContext, ArtifactEntity, NugetODataFilterVisitorImpl>
{

    public NugetODataFilterQueryParser(String query)
    {
        super(createParser(CharStreams.fromString(query)));
    }

    public static Parser createParser(CharStream is)
    {
        NugetODataFilterLexer lexer = new NugetODataFilterLexer(is);
        CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
        return new NugetODataFilterParser(commonTokenStream);
    }

    @Override
    protected NugetODataFilterVisitorImpl createTreeVisitor()
    {
        return new NugetODataFilterVisitorImpl();
    }

    @Override
    protected NugetODataFilterParser.FilterContext parseQueryTree(Parser parser)
    {
        return ((NugetODataFilterParser) parser).filter();
    }
}
