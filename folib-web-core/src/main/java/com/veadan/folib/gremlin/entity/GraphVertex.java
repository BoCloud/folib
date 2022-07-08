package com.veadan.folib.gremlin.entity;

import lombok.Data;


/**
 * @Author: haifeng
 * @Date: 2019-08-30 16:50
 */
@Data
public class GraphVertex extends Element {
    private boolean draggable = true;
    private int category = 0;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        GraphVertex vertex = (GraphVertex) o;
        return this.getId().equals(vertex.getId());
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
