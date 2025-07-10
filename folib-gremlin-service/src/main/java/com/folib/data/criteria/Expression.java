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

/**
 * @author veadan
 *
 */
public class Expression
{

    private String property;
    private ExpOperator operator = ExpOperator.EQ;
    private Object value;

    Expression()
    {

    }

    public Expression(String property,
                      Object value)
    {
        this(property, ExpOperator.EQ, value);
    }

    public Expression(String property,
                      ExpOperator operator,
                      Object value)
    {
        super();
        this.property = property;
        this.operator = operator;
        this.value = value;
    }

    public String getProperty()
    {
        return property;
    }

    public void setProperty(String property)
    {
        this.property = property;
    }

    public ExpOperator getOperator()
    {
        return operator;
    }

    public void setOperator(ExpOperator operator)
    {
        this.operator = operator;
    }

    public Object getValue()
    {
        return value;
    }

    public void setValue(Object value)
    {
        this.value = value;
    }

    public enum ExpOperator
    {
        EQ, GE, LE, CONTAINS, LIKE, IS_NULL, IS_NOT_NULL;

        public Expression of(String property,
                             Object value)
        {
            return new Expression(property, this, value);
        }

        public Expression of(String property)
        {
            return new Expression(property, this, null);
        }
        
    }
    
}
