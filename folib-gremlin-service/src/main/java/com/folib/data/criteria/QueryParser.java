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
package com.folib.data.criteria;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.RuleContext;
import org.antlr.v4.runtime.misc.Utils;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.antlr.v4.runtime.tree.Trees;
import com.folib.data.domain.DomainObject;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 *
 */
public abstract class QueryParser<T extends ParseTree, E extends DomainObject, V extends ParseTreeVisitor<Selector<E>>>
        extends BaseErrorListener implements ParseTreeListener
{

    @SuppressWarnings("unused")
    private static final Logger logger = LoggerFactory.getLogger(QueryParser.class);

    private final Parser parser;

    private List<String> ruleNames;

    private Map<RuleContext, ArrayList<String>> stack = new HashMap<RuleContext, ArrayList<String>>();

    private StringBuilder treeBuilder = new StringBuilder();

    private Map<Pair<Integer, Integer>, String> errorMap = new TreeMap<>();

    public QueryParser(Parser parser)
    {
        super();
        this.parser = parser;
        init(parser);
    }

    private void init(Parser parser)
    {
        ((Lexer) parser.getTokenStream().getTokenSource()).addErrorListener(this);
        parser.addErrorListener(this);
        ruleNames = Arrays.asList(parser.getRuleNames());
    }

    public Selector<E> parseQuery()
    {
        T queryContext = parseQueryTree(parser);
        ParseTreeWalker.DEFAULT.walk(this, queryContext);
        
        if (hasErrors())
        {
            throw new QueryParserException(getMessage());
        }

        V visitor = createTreeVisitor();
        return visitor.visit(queryContext);
    }

    protected abstract V createTreeVisitor();

    protected abstract T parseQueryTree(Parser parser);

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer,
                            Object offendingSymbol,
                            int line,
                            int position,
                            String msg,
                            RecognitionException e)
    {
        errorMap.put(Pair.with(line, position), msg);
    }

    public Map<Pair<Integer, Integer>, String> getErrors()
    {
        return errorMap;
    }

    public String getMessage()
    {
        return errorMap.entrySet()
                       .stream()
                       .map(entry -> String.format("AQL syntax error at [%s:%s], cause [%s]",
                                                   entry.getKey().getValue0(), entry.getKey().getValue1(),
                                                   entry.getValue()))
                       .collect(Collectors.joining("\n"));
    }

    public Boolean hasErrors()
    {
        return !errorMap.isEmpty();
    }

    @Override
    public void visitTerminal(TerminalNode node)
    {
        String text = Utils.escapeWhitespace(Trees.getNodeText(node, ruleNames), false);
        if (text.startsWith(" ") || text.endsWith(" "))
        {
            text = "'" + text + "'";
        }
        stack.get(node.getParent()).add(text);
    }

    @Override
    public void visitErrorNode(ErrorNode node)
    {
        stack.get(node.getParent()).add(Utils.escapeWhitespace(Trees.getNodeText(node, ruleNames), false));
    }

    @Override
    public void enterEveryRule(ParserRuleContext ctx)
    {
        if (!stack.containsKey(ctx.parent))
        {
            stack.put(ctx.parent, new ArrayList<String>());
        }
        if (!stack.containsKey(ctx))
        {
            stack.put(ctx, new ArrayList<String>());
        }

        final StringBuilder sb = new StringBuilder();
        int ruleIndex = ctx.getRuleIndex();
        String ruleName;
        if (ruleIndex >= 0 && ruleIndex < ruleNames.size())
        {
            ruleName = ruleNames.get(ruleIndex);
        }
        else
        {
            ruleName = Integer.toString(ruleIndex);
        }
        sb.append(ruleName);
        stack.get(ctx).add(sb.toString());
    }

    @Override
    public void exitEveryRule(ParserRuleContext ctx)
    {
        ArrayList<String> ruleStack = stack.remove(ctx);
        StringBuilder sb = new StringBuilder();
        boolean brackit = ruleStack.size() > 1;
        if (brackit)
        {
            sb.append("(");
        }
        sb.append(ruleStack.get(0));
        for (int i = 1; i < ruleStack.size(); i++)
        {
            sb.append(" ");
            sb.append(ruleStack.get(i));
        }
        if (brackit)
        {
            sb.append(")");
        }
        if (sb.length() < 80)
        {
            stack.get(ctx.parent).add(sb.toString());
        }
        else
        {
            // Current line is too long, regenerate it using 1 line per item.
            sb.setLength(0);
            if (brackit)
            {
                sb.append("(");
            }
            if (!ruleStack.isEmpty())
            {
                sb.append(ruleStack.remove(0)).append("\r\n");
            }
            while (!ruleStack.isEmpty())
            {
                sb.append(indent(ruleStack.remove(0))).append("\r\n");
            }
            if (brackit)
            {
                sb.append(")");
            }
            stack.get(ctx.parent).add(sb.toString());
        }
        if (ctx.parent == null)
        {
            treeBuilder.append(sb.toString());
        }
    }

    static String indent(String input)
    {
        return "  " + input.replaceAll("\r\n(.)", "\r\n  $1");
    }

    @Override
    public String toString()
    {
        return treeBuilder.toString();
    }

}
