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
