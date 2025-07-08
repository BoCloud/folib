package com.folib.gremlin.service;

import com.folib.gremlin.common.ArtifactsResult;
import com.folib.gremlin.entity.QueryResult;
import com.folib.gremlin.entity.vo.PropertyVo;
import com.folib.scanner.common.msg.TableResultResponse;

/**
 * @Author: haifeng
 * @Date: 2019-08-30 16:49
 */

public interface QueryService {


    QueryResult query(String host, int port, String gremlin, String sourceName);

    PropertyVo getValueMap(String host, int port, String sourceName, String id, boolean vertex);

    TableResultResponse<ArtifactsResult> queryArtifacts(String host, int port, int pageNum, int pageSize, long artifactSize);

}
