package com.veadan.folib.services;

import com.veadan.folib.dispatch.ClusterDispatchNodeDto;

import java.io.IOException;

/**
 * 分发配置服务service
 *
 * @author qijianping
 */
public interface ClusterDispatchManagementService {
    void createClusterNode(ClusterDispatchNodeDto clusterDispatchNodeDto) throws IOException;

    void updateClusterNode(ClusterDispatchNodeDto clusterDispatchNodeDto);

    void deleteClusterNode(ClusterDispatchNodeDto clusterDispatchNodeDto) throws IOException;
}
