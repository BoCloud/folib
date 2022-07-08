package com.veadan.folib.gremlin.service;

import com.veadan.folib.gremlin.entity.QueryResult;
import com.veadan.folib.gremlin.entity.vo.PropertyVo;
import org.springframework.stereotype.Component;

/**
 * @Author: haifeng
 * @Date: 2019-08-30 16:49
 */

public interface QueryService {


    QueryResult query(String host, int port, String gremlin, String sourceName);

    PropertyVo getValueMap(String host, int port, String sourceName, String id, boolean vertex);

}
