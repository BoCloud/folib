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
package com.folib.gremlin.service.impl;

import cn.hutool.core.date.DateUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.folib.gremlin.common.ArtifactsResult;
import com.folib.gremlin.common.Constant;
import com.folib.gremlin.component.ClusterCache;
import com.folib.gremlin.entity.*;
import com.folib.gremlin.entity.vo.PropertyVo;
import com.folib.gremlin.service.QueryService;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.util.FileSizeConvertUtils;
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


    public TableResultResponse<ArtifactsResult> queryArtifacts(String host, int port, int pageNum, int pageSize, long artifactSize){

        int totalCount = getArtifactSize(host, port,artifactSize);
        int pageCount =   (totalCount % pageSize == 0) ? (totalCount /  pageSize) : (totalCount / pageSize + 1);
        int page = (Math.min(pageCount, pageNum)-1) * pageSize;
        if(totalCount == 0){
            return new TableResultResponse<ArtifactsResult>(0,new ArrayList<ArtifactsResult>() );
        }
        int ende = Math.min(pageNum * pageSize, totalCount);
        String gremlin =  String.format("g.V().hasLabel(\"Artifact\").has(\"sizeInBytes\", gt(%s)).order().by(\"sizeInBytes\", desc).range(%s,%s).valueMap(true)",artifactSize,page,ende);
        Client client = getClient(host, port);
        ResultSet set = client.submit(gremlin);
        Iterator<Result> iterator = set.iterator();
        List<ArtifactsResult> list = new ArrayList<>();
        while (iterator.hasNext()) {

            Result next = iterator.next();
            Object object = next.getObject();
            JSONObject jsonObject = JSON.parseObject(JSON.toJSONString(object));
            ArtifactsResult result = new ArtifactsResult();
            //"artifactName", "artifactPath", "storageId", "repositoryId", "sizeInBytes"
            result.setArtifactName(jsonObject.getJSONArray("artifactName").get(0).toString());
            result.setArtifactPath(jsonObject.getJSONArray("artifactPath").get(0).toString());
            result.setStorageId(jsonObject.getJSONArray("storageId").get(0).toString());
            result.setRepositoryId(jsonObject.getJSONArray("repositoryId").get(0).toString());
            String sizeInBytes = "0";
            JSONArray array = jsonObject.getJSONArray("sizeInBytes");
            if (array != null && array.size() > 0) {
                Object firstElement = array.get(0);
                if (firstElement != null) {
                    sizeInBytes = firstElement.toString();
                }
            }
            result.setSizeInBytes(FileSizeConvertUtils.convertBytesWithDecimal(Long.parseLong(sizeInBytes), "MB"));
            result.setPath(String.format("%s/%s/%s",result.getStorageId(),result.getRepositoryId(),result.getArtifactPath()));
            list.add(result);
        }
        return new TableResultResponse<ArtifactsResult>(totalCount,  list);
    }

    public  int getArtifactSize(String host, int port,long artifactSize){
        String gremlin = String.format("g.V().hasLabel(\"Artifact\").has(\"sizeInBytes\", gt(%s)).order().by(id).count()",artifactSize);
        Client client = getClient(host, port);
        ResultSet set = client.submit(gremlin);
        Iterator<Result> iterator = set.iterator();
        if (iterator.hasNext()) {
            Result next = iterator.next();
            return Integer.valueOf(next.getObject().toString());
        }
        return 0;
    }
}
