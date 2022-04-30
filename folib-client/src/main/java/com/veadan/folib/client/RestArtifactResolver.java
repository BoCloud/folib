package com.veadan.folib.client;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Feature;
import javax.ws.rs.core.Response;
import java.io.Closeable;

import org.apache.http.client.config.RequestConfig;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author mtodorov
 */
public class RestArtifactResolver
        implements Closeable
{

    private static final Logger logger = LoggerFactory.getLogger(RestArtifactResolver.class);

    private final String repositoryBaseUrl;
    private final Client client;
    private Feature authentication;
    private RemoteRepositoryRetryArtifactDownloadConfiguration configuration;

    public RestArtifactResolver(Client client,
                                String repositoryBaseUrl,
                                RemoteRepositoryRetryArtifactDownloadConfiguration configuration)
    {
        this.client = client;
        this.repositoryBaseUrl = normalize(repositoryBaseUrl);
        this.configuration = configuration;
    }

    public RestArtifactResolver(Client client,
                                String repositoryBaseUrl,
                                RemoteRepositoryRetryArtifactDownloadConfiguration configuration,
                                Feature authentication)
    {
        this(client, repositoryBaseUrl, configuration);
        this.authentication = authentication;
    }
    
    public RemoteRepositoryRetryArtifactDownloadConfiguration getConfiguration()
    {
        return configuration;
    }
    
    public boolean isAlive()
    {
        return true;
    }

    @Override
    public void close()
    {
        if (client != null)
        {
            client.close();
        }
    }

    public CloseableRestResponse get(String path)
    {
        return get(path, 0);
    }

    public CloseableRestResponse get(String path,
                                     long offset)
    {
        String url = escapeUrl(path);

        logger.debug("Getting {}...", url);

        WebTarget resource = new WebTargetBuilder(url).withAuthentication()
                                                      .customRequestConfig()
                                                      .build();

        Invocation.Builder request = resource.request();
        Response response;

        if (offset > 0)
        {
            response = request.header("Range", "bytes=" + offset + "-").get();
        }
        else
        {
            response = request.get();
        }

        return new CloseableRestResponse(response);
    }

    public CloseableRestResponse head(String path)
    {
        String url = escapeUrl(path);

        logger.debug("Heading {}...", url);

        WebTarget resource = new WebTargetBuilder(url)
                                     .withAuthentication()
                                     .customRequestConfig()
                                     .build();

        return new CloseableRestResponse(resource.request().head());
    }

    private String escapeUrl(String path)
    {
        String baseUrl = repositoryBaseUrl + (repositoryBaseUrl.endsWith("/") ? "" : "/");
        String p = (path.startsWith("/") ? path.substring(1, path.length()) : path);

        return baseUrl + p;
    }

    private String normalize(String repositoryBaseUrl)
    {
        return repositoryBaseUrl.endsWith("/") ? repositoryBaseUrl : repositoryBaseUrl + "/";
    }

    private class WebTargetBuilder
    {

        private final WebTarget target;

        private WebTargetBuilder(String uri)
        {
            this.target = client.target(uri);
        }

        private WebTargetBuilder withAuthentication()
        {
            if (authentication != null)
            {
                target.register(authentication);
            }
            return this;
        }

        private WebTargetBuilder customRequestConfig()
        {
            target.property(ApacheClientProperties.REQUEST_CONFIG,
                            RequestConfig.custom().setCircularRedirectsAllowed(true).build());
            return this;
        }

        public WebTarget build()
        {
            return target;
        }
    }

}
