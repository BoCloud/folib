package com.veadan.folib.services.impl;

import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.services.ClusterDispatchManagementService;
import com.veadan.folib.services.ConfigurationManagementService;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.IOException;

/**
 * 分发配置服务实现
 *
 * @author qijianping
 */
@Service
public class ClusterDispatchManagementServiceImpl implements ClusterDispatchManagementService {

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Override
    public void createClusterNode(ClusterDispatchNodeDto clusterDispatchNodeDto) throws IOException {
        configurationManagementService.createClusterDispatchConfig(clusterDispatchNodeDto);
    }

    @Override
    public void updateClusterNode(ClusterDispatchNodeDto clusterDispatchNodeDto) {

    }

    @Override
    public void deleteClusterNode(ClusterDispatchNodeDto clusterDispatchNodeDto) throws IOException {
        configurationManagementService.removeClusterDispatchConfig(clusterDispatchNodeDto);
    }
}
