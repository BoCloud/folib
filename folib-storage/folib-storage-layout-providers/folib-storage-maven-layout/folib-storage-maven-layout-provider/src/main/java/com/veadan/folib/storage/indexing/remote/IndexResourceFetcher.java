package com.veadan.folib.storage.indexing.remote;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.text.MessageFormat;

import com.google.common.io.Closeables;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.maven.index.updater.ResourceFetcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class IndexResourceFetcher
        implements ResourceFetcher, Closeable
{

    private static final String INDEX_URI_PATTERN = "{0}/.index/{1}";

    private static final Logger logger = LoggerFactory.getLogger(IndexResourceFetcher.class);

    private final String repositoryBaseUrl;

    private final CloseableHttpClient client;

    private CloseableHttpResponse response;

    public IndexResourceFetcher(String repositoryBaseUrl,
                                CloseableHttpClient client)
    {
        this.repositoryBaseUrl = StringUtils.removeEnd(repositoryBaseUrl, "/");
        this.client = client;
    }

    @Override
    public void connect(String indexContextId,
                        String indexUpdateUrl)
    {
        // ignored
    }

    @Override
    public void disconnect()
            throws IOException
    {
        close();
    }

    @Override
    public InputStream retrieve(String indexName)
            throws IOException
    {
        final String uri = MessageFormat.format(INDEX_URI_PATTERN, repositoryBaseUrl, indexName);

        logger.info("Getting {}...", uri);

        InputStream result = null;

        response = client.execute(new HttpGet(uri));

        HttpEntity httpEntity = response.getEntity();
        if (httpEntity != null)
        {
            result = httpEntity.getContent();
        }
        return result;
    }

    @Override
    public void close()
            throws IOException
    {
        Closeables.close(response, true);
        Closeables.close(client, true);
    }
}
