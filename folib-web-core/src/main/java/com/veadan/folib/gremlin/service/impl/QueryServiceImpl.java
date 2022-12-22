package com.veadan.folib.gremlin.service.impl;

import cn.hutool.core.date.DateUtil;
import com.veadan.folib.gremlin.common.Constant;
import com.veadan.folib.gremlin.component.ClusterCache;
import com.veadan.folib.gremlin.entity.*;
import com.veadan.folib.gremlin.entity.vo.PropertyVo;
import com.veadan.folib.gremlin.service.QueryService;
import org.apache.tinkerpop.gremlin.driver.Client;
import org.apache.tinkerpop.gremlin.driver.Result;
import org.apache.tinkerpop.gremlin.driver.ResultSet;
import org.apache.tinkerpop.gremlin.process.traversal.Path;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.JanusGraph;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * @Author: haifeng
 * @Date: 2019-08-30 16:49
 */
@Service
public class QueryServiceImpl implements QueryService {

    @Autowired
    private ClusterCache clusterCache;

    @Autowired
    private JanusGraph janusGraph;

    private Client getClient(String host, int port) {
        Client client = clusterCache.get(host, port);
        if (client == null) {
            client = clusterCache.put(host, port);
        }
        return client;
    }

    @Override
    public QueryResult query(String host, int port, String gremlin, String sourceName) {

        Client client = getClient(host, port);
        ResultSet set = client.submit(gremlin);
        QueryResult result = new QueryResult();
        StringBuilder builder = new StringBuilder();
        Iterator<Result> iterator = set.iterator();
        String errorMessage = null;
        try {
            while (iterator.hasNext()) {
                Result next = iterator.next();
                builder.append(next.getString()).append(Constant.RESULT_SPLIT);
                Object obj = next.getObject();
                if (obj instanceof Vertex) {
                    Vertex vertex = next.getVertex();
                    GraphVertex graphVertex = convert(vertex);
                    result.getVertices().add(graphVertex);
                } else if (obj instanceof Edge) {
                    Edge edge = next.getEdge();
                    GraphEdge graphEdge = convert(edge);
                    result.getEdges().add(graphEdge);
                } else if (obj instanceof Path) {
                    Path path = next.getPath();
                    for (Object next1 : path) {
                        if (next1 instanceof Vertex) {
                            Vertex vertex = (Vertex) next1;
                            GraphVertex graphVertex = convert(vertex);
                            result.getVertices().add(graphVertex);
                        } else {
                            Edge edge = (Edge) next1;
                            GraphEdge graphEdge = convert(edge);
                            result.getEdges().add(graphEdge);
                        }
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            errorMessage = e.getMessage();
        }
        result.merge();
        if (errorMessage == null) {
            String rs = builder.toString().isEmpty() ? "无结果" : builder.toString();
            result.setResult(rs);
        } else {
            result.setResult(errorMessage);
        }
        return result;
    }


    @Override
    public PropertyVo getValueMap(String host, int port, String sourceName, String id, boolean isVertex) {
        String gremlin = isVertex ? String.format("%s.V('%s').valueMap()", sourceName, id) : String.format("%s.E('%s').valueMap()", sourceName, id);
        Client client = getClient(host, port);
        ResultSet set = client.submit(gremlin);
        Iterator<Result> iterator = set.iterator();
        if (iterator.hasNext()) {
            Element element = new Element();
            Result next = iterator.next();
            Object object = next.getObject();
            LinkedHashMap<String, Object> list = (LinkedHashMap<String, Object>) object;
            for (String key : list.keySet()) {
                GraphProperty graphProperty = new GraphProperty();
                graphProperty.setKey(key);
                Object values = list.get(key);
                if (values instanceof List) {
                    for (Object value : (List) values) {
                        if (value instanceof Date) {
                            graphProperty.addValue(DateUtil.formatDateTime((Date) value));
                        } else {
                            graphProperty.addValue(value.toString());
                        }
                    }
                    element.putProperty(graphProperty);
                } else if (values instanceof Set) {
                    for (Object value : (Set) values) {
                        if (value instanceof Date) {
                            graphProperty.addValue(DateUtil.formatDateTime((Date) value));
                        } else {
                            graphProperty.addValue(value.toString());
                        }
                    }
                    element.putProperty(graphProperty);
                } else {
                    if (values instanceof Date) {
                        graphProperty.addValue(DateUtil.formatDateTime((Date) values));
                    } else {
                        graphProperty.addValue(values.toString());
                    }
                    element.putProperty(graphProperty);
                }
            }
            PropertyVo propertyVo = new PropertyVo(element);
            propertyVo.setVertex(isVertex);
            return propertyVo;
        } else {
            return null;
        }
    }

    private GraphEdge convert(Edge edge) {
        GraphEdge graphEdge = new GraphEdge();
        graphEdge.setId(edge.id().toString());
        graphEdge.setLabel(edge.label());

        Vertex inVertex = edge.inVertex();
        Vertex outVertex = edge.outVertex();

        graphEdge.setFrom(outVertex.id().toString());
        graphEdge.setTo(inVertex.id().toString());

        GraphVertex intGraphVertex = new GraphVertex();
        intGraphVertex.setId(inVertex.id().toString());
        intGraphVertex.setLabel(inVertex.label());

        GraphVertex outGraphVertex = new GraphVertex();
        outGraphVertex.setId(outVertex.id().toString());
        outGraphVertex.setLabel(outVertex.id().toString());

        graphEdge.setSource(convert(outVertex));
        graphEdge.setTarget(convert(inVertex));

        return graphEdge;
    }

    private GraphVertex convert(Vertex vertex) {
        GraphVertex graphVertex = new GraphVertex();
        graphVertex.setId(vertex.id().toString());
        graphVertex.setLabel(vertex.label());
        return graphVertex;
    }

}
