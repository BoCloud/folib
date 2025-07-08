package com.folib.gremlin.entity;

import java.util.ArrayList;
import java.util.List;

/**
 * @Author: haifeng
 * @Date: 2019-08-29 18:21
 */


public class Element {
    private String id;
    private String label;
    private List<GraphProperty> properties = new ArrayList<>(10);

    public void putProperty(GraphProperty property) {
        this.properties.add(property);
    }

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

    public List<GraphProperty> getProperties() {
        return properties;
    }

    public void setProperties(List<GraphProperty> properties) {
        this.properties = properties;
    }
}
