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
package com.folib.providers.repository.proxied;

import cn.hutool.core.util.StrUtil;
import cn.hutool.extra.spring.SpringUtil;
import com.google.common.collect.Lists;
import com.folib.artifact.ArtifactNotFoundException;
import com.folib.client.CloseableRestResponse;
import com.folib.client.RestArtifactResolver;
import com.folib.components.DistributedCacheComponent;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import javax.ws.rs.core.Response;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

@Slf4j
public class RemoteArtifactStreamFetcher
{

    private RestArtifactResolver client;

    public RemoteArtifactStreamFetcher(RestArtifactResolver client)
    {
        super();
        this.client = client;
    }

    public InputStream getInputStream(long offset,
                                      RepositoryPath artifactPath)
        throws IOException
    {
        URI resource = getRestClientResourcePath(artifactPath);
        RemoteArtifactInputStream remoteArtifactInputStream = new RemoteArtifactInputStream(resource, offset);
        Response response = remoteArtifactInputStream.getConnection().getResponse();
        if (Objects.nonNull(response)) {
            String size = response.getHeaderString("Content-Length");
            if (StringUtils.isNotBlank(size) && StrUtil.isNumeric(size)) {
                artifactPath.setSize(Long.valueOf(size));
                log.info("RepositoryPath [{}] Content-Length [{}]", artifactPath.toString(), size);
            }
        }
        return remoteArtifactInputStream;
    }

    public String getHead(RepositoryPath repositoryPath)
        throws IOException
    {
        URI resource = getRestClientResourcePath(repositoryPath);
        try (final CloseableRestResponse closeableRestResponse = client.head(resource.toString()))
        {
            final Response response = closeableRestResponse.getResponse();

            if (response.getStatus() != 200 || response.getEntity() == null)
            {
                return null;
            }

            return response.getHeaderString("Accept-Ranges");
        }

    }

    private URI getRestClientResourcePath(final RepositoryPath artifactPath)
        throws IOException
    {
        return RepositoryFiles.resolveResource(artifactPath);
    }

    private CloseableRestResponse getConnection(URI resource,
                                                long offset)
        throws IOException
    {
        return getConnection(resource, "", offset);
    }

    private CloseableRestResponse getConnection(URI resource, String url, long offset) throws IOException {
        if (StringUtils.isBlank(url)) {
            url = resource.toString();
        }
        CloseableRestResponse connection = client.get(url, offset);

        Response response = connection.getResponse();

        if (response.getStatus() == 404)
        {
            terminateConnection(connection);
            log.warn("Artifact not found response for [{}]", response.toString());
            throw new ArtifactNotFoundException(resource);
        }
        if (response.getStatus() != 200 || response.getEntity() == null)
        {
            terminateConnection(connection);
            if (response.getStatus() == 307) {
                String redirectUrl = response.getHeaderString("Location");
                if (StringUtils.isBlank(redirectUrl)) {
                    throw new IOException(String.format("Unreadable response for %s. Response status is %s",
                            response.toString(), response.getStatus()));
                }
                client.setTargetUrl(redirectUrl);
                if (Objects.nonNull(client.getHeaders())) {
                    getRemoveHeaderList().forEach(header -> {
                        client.getHeaders().remove(header);
                    });
                }
                return getConnection(resource, redirectUrl, offset);
            }
            throw new IOException(String.format("Unreadable response for %s. Response status is %s",
                    response.toString(), response.getStatus()));
        }

        return connection;
    }

    private void terminateConnection(CloseableRestResponse connection)
    {
        try
        {
            connection.close();
        }
        catch (Exception e)
        {
            // ignore
        }
    }

    public class RemoteArtifactInputStream extends InputStream
    {

        private URI resource;
        private long offset;
        private CloseableRestResponse connection;
        private InputStream target;

        public RemoteArtifactInputStream(URI resource,
                                         long offset)
        {
            this.resource = resource;
            this.offset = offset;
        }

        public CloseableRestResponse getConnection()
            throws IOException
        {
            if (connection != null)
            {
                return connection;
            }
            return connection = RemoteArtifactStreamFetcher.this.getConnection(resource, offset);
        }

        private InputStream getTarget()
            throws IOException
        {
            if (target != null)
            {
                return target;
            }

            target = getConnection().getResponse().readEntity(InputStream.class);
            if (target == null)
            {
                throw new IOException(String.format("Unexpected null as InputStream response for %s.",
                                                    resource));
            }

            return target;
        }

        @Override
        public int read()
            throws IOException
        {
            return getTarget().read();
        }

        @Override
        public int read(byte[] b)
            throws IOException
        {
            return getTarget().read(b);
        }

        @Override
        public int read(byte[] b,
                        int off,
                        int len)
            throws IOException
        {
            return getTarget().read(b, off, len);
        }

        @Override
        public long skip(long n)
            throws IOException
        {
            return getTarget().skip(n);
        }

        @Override
        public int available()
            throws IOException
        {
            return getTarget().available();
        }

        @Override
        public void mark(int readlimit)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        public void reset()
            throws IOException
        {
            getTarget().reset();
        }

        @Override
        public boolean markSupported()
        {
            return false;
        }

        @Override
        public void close()
            throws IOException
        {
            try
            {
                if (target != null)
                {
                    target.close();
                }
            } finally
            {
                closeConnection();
            }
        }

        private void closeConnection()
            throws IOException
        {
            try
            {
                if (connection != null)
                {
                    connection.close();
                }
            }
            catch (IOException e)
            {
                throw e;
            }
            catch (Exception e)
            {
                throw new IOException(e);
            }
        }

    }

    private List<String> getRemoveHeaderList() {
        List<String> removeHeaderList = Lists.newArrayList("Authorization", "authorization");
        String key = "REMOTE_REPOSITORY_REMOVE_FOLLOW_REDIRECTS_HEADER";
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        String values = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(values)) {
            Arrays.asList(values.split(",")).forEach(item -> {
                if (StringUtils.isNotBlank(item) && !removeHeaderList.contains(item)) {
                    removeHeaderList.add(item);
                }
            });
        }
        log.info("Remove follow redirects header [{}]", String.join(",", removeHeaderList));
        return removeHeaderList;
    }
}
