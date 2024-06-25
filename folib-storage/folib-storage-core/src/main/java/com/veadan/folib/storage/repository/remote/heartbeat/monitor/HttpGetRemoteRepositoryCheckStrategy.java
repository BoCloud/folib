package com.veadan.folib.storage.repository.remote.heartbeat.monitor;

import com.google.common.collect.Lists;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpStatus;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Component
class HttpGetRemoteRepositoryCheckStrategy
        implements RemoteRepositoryHeartbeatMonitorStrategy {

    private static final Logger logger = LoggerFactory.getLogger(HttpGetRemoteRepositoryCheckStrategy.class);

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Override
    public boolean isAlive(String remoteRepositoryUrl) {
        boolean response = false;
        long startTime = System.currentTimeMillis();
        try {
            List<Integer> allowAccessList = getAllowAccessList();
            try (final CloseableHttpClient httpClient = proxyRepositoryConnectionPoolConfigurationService.getHttpClient()) {
                RequestConfig requestConfig = RequestConfig.custom()
                        .setConnectTimeout(10000)
                        .build();
                HttpGet httpGet = new HttpGet(remoteRepositoryUrl);
                httpGet.setConfig(requestConfig);
                try (final CloseableHttpResponse httpResponse = httpClient.execute(httpGet)) {

                    int statusCode = httpResponse.getStatusLine().getStatusCode();
                    logger.info("The remote repository url [{}] allow access [{}] response status [{}]", remoteRepositoryUrl, allowAccessList.stream().map(String::valueOf).collect(Collectors.joining(",")), statusCode);
                    response = allowAccessList.contains(statusCode) || String.valueOf(statusCode).startsWith("2");
                }
            }
        } catch (Exception e) {
            logger.error("Problem executing HTTP GET request to {}", remoteRepositoryUrl, e);
            response = false;
        }
        logger.info("The remote repository url [{}] take time [{}] ms", remoteRepositoryUrl, System.currentTimeMillis() - startTime);
        return response;
    }

    private List<Integer> getAllowAccessList() {
        List<Integer> allowAccessList = Lists.newArrayList(HttpStatus.SC_OK, HttpStatus.SC_MOVED_PERMANENTLY, HttpStatus.SC_MOVED_TEMPORARILY, HttpStatus.SC_UNAUTHORIZED, HttpStatus.SC_NOT_FOUND);
        String key = "REMOTE_REPOSITORY_ALLOW_ACCESS_CODE";
        String values = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(values)) {
            Arrays.asList(values.split(",")).forEach(item -> {
                int statusCode = Integer.parseInt(item);
                if (!allowAccessList.contains(statusCode)) {
                    allowAccessList.add(statusCode);
                }
            });
        }
        return allowAccessList;
    }
}
