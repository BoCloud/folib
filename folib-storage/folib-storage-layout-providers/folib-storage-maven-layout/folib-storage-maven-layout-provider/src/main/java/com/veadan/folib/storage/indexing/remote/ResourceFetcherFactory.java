package com.veadan.folib.storage.indexing.remote;

import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.maven.index.updater.ResourceFetcher;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ResourceFetcherFactory
{

    public ResourceFetcher createIndexResourceFetcher(String repositoryBaseUrl,
                                                      CloseableHttpClient client)
    {
        return new IndexResourceFetcher(repositoryBaseUrl, client);
    }
}
