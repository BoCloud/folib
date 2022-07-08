package com.veadan.folib.gremlin.entity;

import lombok.Data;

/**
 * @Author: haifeng
 * @Date: 2019-08-30 16:50
 */

public class GraphEdge extends Element {

    private String from;

    private String to;


    private GraphVertex source;

    private GraphVertex target;

    public String getFrom() {
        return from;
    }

    public void setFrom(String from) {
        this.from = from;
    }

    public String getTo() {
        return to;
    }

    public void setTo(String to) {
        this.to = to;
    }

    public GraphVertex getSource() {
        return source;
    }

    public void setSource(GraphVertex source) {
        this.source = source;
    }

    public GraphVertex getTarget() {
        return target;
    }

    public void setTarget(GraphVertex target) {
        this.target = target;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        GraphEdge edge = (GraphEdge) o;
        return this.getId().equals(edge.getId());
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
