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
package com.folib.gremlin.entity.vo;

import com.folib.gremlin.entity.Element;
import com.folib.gremlin.entity.GraphProperty;
import com.folib.gremlin.entity.KeyValue;

import java.util.ArrayList;
import java.util.List;

/**
 * @Author: haifeng
 * @Date: 2019/11/26 16:27
 */

public class PropertyVo {
    private String id;
    private String label;
    private boolean vertex;
    List<KeyValue> keyValues = new ArrayList<KeyValue>(50);

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public boolean isVertex() {
        return vertex;
    }

    public void setVertex(boolean vertex) {
        this.vertex = vertex;
    }

    public List<KeyValue> getKeyValues() {
        return keyValues;
    }

    public void setKeyValues(List<KeyValue> keyValues) {
        this.keyValues = keyValues;
    }

    public PropertyVo(Element element) {
        this.id = element.getId();
        this.label = element.getLabel();
        List<GraphProperty> properties = element.getProperties();
        for (GraphProperty property : properties) {
            String key = property.getKey();
            List<String> value = property.getValue();
            for (String v : value) {
                keyValues.add(new KeyValue(key, v));
            }
        }
    }
}
