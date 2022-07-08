package com.veadan.folib.gremlin.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

import java.util.ArrayList;
import java.util.List;

/**
 * @Author: haifeng
 * @Date: 2019-09-05 11:46
 */
public class GraphProperty {
    private VertexProperty.Cardinality cardinality;
    private String key;
    private List<String> value = new ArrayList<>(5);

    public void addValue(String value) {
        this.value.add(value);
    }

    public VertexProperty.Cardinality getCardinality() {
        return cardinality;
    }

    public void setCardinality(VertexProperty.Cardinality cardinality) {
        this.cardinality = cardinality;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public List<String> getValue() {
        return value;
    }

    public void setValue(List<String> value) {
        this.value = value;
    }
}
