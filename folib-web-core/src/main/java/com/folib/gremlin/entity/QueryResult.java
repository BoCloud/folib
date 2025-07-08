package com.folib.gremlin.entity;

import java.util.HashSet;
import java.util.Set;

/**
 * @Author: haifeng
 * @Date: 2019-08-30 12:36
 */

public class QueryResult {
    private Set<Element> vertices = new HashSet<>(20);
    private Set<Element> edges = new HashSet<>(20);
    private String result;

    public Set<Element> getVertices() {
        return vertices;
    }

    public void setVertices(Set<Element> vertices) {
        this.vertices = vertices;
    }

    public Set<Element> getEdges() {
        return edges;
    }

    public void setEdges(Set<Element> edges) {
        this.edges = edges;
    }

    public String getResult() {
        return result;
    }

    public void setResult(String result) {
        this.result = result;
    }

    /**
     * 合并边和顶点的数据
     */
    public void merge() {
        for (Element edge : edges) {
            GraphEdge e = (GraphEdge) edge;
            this.vertices.add(e.getSource());
            this.vertices.add(e.getTarget());
        }
    }
}
