package com.veadan.folib.storage.repository.remote.heartbeat.monitor;

import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;

import javax.inject.Inject;
import java.io.IOException;

import org.apache.http.HttpStatus;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
class HttpGetRemoteRepositoryCheckStrategy
        implements RemoteRepositoryHeartbeatMonitorStrategy
{

    private static final Logger logger = LoggerFactory.getLogger(HttpGetRemoteRepositoryCheckStrategy.class);

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Override
    public boolean isAlive(String remoteRepositoryUrl)
    {
        boolean response = false;
        try
        {
            try (final CloseableHttpClient httpClient = proxyRepositoryConnectionPoolConfigurationService.getHttpClient())
            {
                try (final CloseableHttpResponse httpResponse = httpClient.execute(new HttpGet(remoteRepositoryUrl)))
                {

                    int statusCode = httpResponse.getStatusLine().getStatusCode();

                    response = HttpStatus.SC_OK == statusCode || HttpStatus.SC_MOVED_PERMANENTLY == statusCode ||
                               HttpStatus.SC_MOVED_TEMPORARILY == statusCode;
                }
            }
        }
        catch (IOException e)
        {
            logger.error("Problem executing HTTP GET request to {}", remoteRepositoryUrl, e);
        }
        finally
        {
            return response;
        }
    }
}
