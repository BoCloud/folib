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

import java.io.Closeable;
import java.io.FilterInputStream;
import java.io.IOException;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.time.StopWatch;
import com.folib.artifact.ArtifactNotFoundException;
import com.folib.client.RemoteRepositoryRetryArtifactDownloadConfiguration;
import com.folib.client.RestArtifactResolver;
import com.folib.providers.io.RepositoryPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 *
 */
public class ProxyRepositoryInputStream extends FilterInputStream
{

    private static final Logger logger = LoggerFactory.getLogger(ProxyRepositoryInputStream.class);

    private RestArtifactResolver client;

    private ThreadLocal<ArtifactCopyContext> artifactCopyContext = new ThreadLocal<>();;

    private ReadTemplate readTemplate = new ReadTemplate();

    RemoteArtifactStreamFetcher remoteArtifactStreamFetcher;

    private RepositoryPath repositoryPath;

    public ProxyRepositoryInputStream(RestArtifactResolver proxyTargetClient,
                                      RepositoryPath path)
        throws IOException
    {
        super(new RemoteArtifactStreamFetcher(proxyTargetClient).getInputStream(0, path));

        this.repositoryPath = path;
        this.client = proxyTargetClient;
        this.remoteArtifactStreamFetcher = new RemoteArtifactStreamFetcher(client);

        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        ArtifactCopyContext context = new ArtifactCopyContext();
        context.setAttempts(1);
        context.setCurrentOffset(0);
        context.setStopWatch(stopWatch);
        artifactCopyContext.set(context);
    }

    @Override
    public int read()
        throws IOException
    {
        return (int) readTemplate.doRead(() -> super.read());
    }

    @Override
    public int read(byte[] b)
        throws IOException
    {
        return (int) readTemplate.doRead(() -> super.read(b));
    }

    @Override
    public int read(byte[] b,
                    int off,
                    int len)
        throws IOException
    {
        return (int) readTemplate.doRead(() -> super.read(b, off, len));
    }

    @Override
    public long skip(long n)
        throws IOException
    {
        return readTemplate.doRead(() -> super.skip(n));
    }

    @Override
    public int available()
        throws IOException
    {
        if (!checkRemoteRepositoryHeartbeat())
        {
            throw new IOException(String.format("Remote repository not avaliable for path [%s] ", repositoryPath));
        }

        return super.available();
    }

    @Override
    public void close()
        throws IOException
    {
        try
        {
            super.close();
        } finally
        {
            this.artifactCopyContext.get().close();
        }

    }

    @Override
    public synchronized void mark(int readlimit)
    {
        throw new UnsupportedOperationException();
    }

    @Override
    public synchronized void reset()
        throws IOException
    {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean markSupported()
    {
        return false;
    }

    private class ReadTemplate
    {
        public long doRead(InputStreamRead f)
            throws IOException
        {
            ArtifactCopyContext ctx = artifactCopyContext.get();

            long offset = 0;
            try
            {
                offset = f.read();
            }
            catch (ArtifactNotFoundException e)
            {
                throw e;
            }
            catch (IOException e)
            {
                offset = retryReadIfPossible(f, e);
            }

            ctx.setCurrentOffset(ctx.getCurrentOffset() + offset);

            return offset;
        }

    }

    private long retryReadIfPossible(InputStreamRead f,
                                     IOException lastException)
        throws IOException
    {
        ArtifactCopyContext ctx = artifactCopyContext.get();
        ctx.setAttempts(ctx.getAttempts() + 1);

        logger.info("Retrying remote stream reading because of [{}]... Attempt number = [{}], Current Offset = [{}] Duration Time = [{}]",
                     lastException, ctx.getAttempts(), ctx.getCurrentOffset(),
                     ctx.getStopWatch());

        finishUnsuccessfullyIfNumberOfAttemptsExceedTheLimit(lastException);
        tryToSleepRequestedAmountOfTimeBetweenAttempts(lastException);
        finishUnsuccessfullyIfTimeoutOccurred(lastException);

        if (!checkRemoteRepositoryHeartbeat())
        {
            retryReadIfPossible(f, lastException);
        }

        if (ctx.getRangeRequestSupported() == null && ctx.getCurrentOffset() > 0)
        {
            ctx.setRangeRequestSupported(isRangeRequestSupported());
        }

        if (Boolean.FALSE.equals(ctx.getRangeRequestSupported()))
        {
            throw new IOException(String.format("Remote resource path [%s] does not support range requests.",
                                                repositoryPath),
                    lastException);
        }

        this.in.close();
        this.in = remoteArtifactStreamFetcher.getInputStream(ctx.getCurrentOffset(), repositoryPath);

        return readTemplate.doRead(f);
    }

    private boolean isRangeRequestSupported()
        throws IOException
    {
        String acceptRangesHeader = remoteArtifactStreamFetcher.getHead(repositoryPath);

        return StringUtils.isNotBlank(acceptRangesHeader) && !"none".equals(acceptRangesHeader);
    }

    private boolean checkRemoteRepositoryHeartbeat()
    {
        return client.isAlive();
    }

    private void finishUnsuccessfullyIfNumberOfAttemptsExceedTheLimit(IOException ex)
        throws IOException
    {
        int maxAllowedNumberOfRetryAttempts = getMaxAllowedNumberOfRetryAttempts();
        if (artifactCopyContext.get().getAttempts() > maxAllowedNumberOfRetryAttempts)
        {
            logger.error("Maximum retry attempts [{}] reached for [{}]",
                         maxAllowedNumberOfRetryAttempts, repositoryPath);
            throw ex;
        }
    }

    private void finishUnsuccessfullyIfTimeoutOccurred(IOException ex)
        throws IOException
    {
        long retryTimeoutMillis = getRetryTimeoutMillis();
        if (artifactCopyContext.get().getStopWatch().getTime() > retryTimeoutMillis)
        {
            logger.error("Timeout of [{}] occurred while reading [{}]",
                         retryTimeoutMillis, repositoryPath);
            throw ex;
        }
    }

    private long getRetryTimeoutMillis()
    {
        return getRetryConfiguration().getTimeoutSeconds() * 1000L;
    }

    private long getSleepMillisTimeBeforeNextAttempt()
    {
        return getRetryConfiguration().getMinAttemptsIntervalSeconds() * 1000L;
    }

    private int getMaxAllowedNumberOfRetryAttempts()
    {
        return getRetryConfiguration().getMaxNumberOfAttempts();
    }

    private void tryToSleepRequestedAmountOfTimeBetweenAttempts(final IOException ex)
        throws IOException
    {
        try
        {
            // TODO: we should use Object.wait() instead
            Thread.sleep(getSleepMillisTimeBeforeNextAttempt());
        }
        catch (final InterruptedException e)
        {
            Thread.currentThread().interrupt();
            logger.error(e.getMessage(), e);
            throw ex;
        }
    }

    private RemoteRepositoryRetryArtifactDownloadConfiguration getRetryConfiguration()
    {
        return client.getConfiguration();
    }

    private class ArtifactCopyContext implements Closeable
    {

        private StopWatch stopWatch;
        private int attempts;
        private long currentOffset;
        private Boolean rangeRequestSupported;

        public StopWatch getStopWatch()
        {
            return stopWatch;
        }

        public void setStopWatch(StopWatch stopWatch)
        {
            this.stopWatch = stopWatch;
        }

        public int getAttempts()
        {
            return attempts;
        }

        public void setAttempts(int attempts)
        {
            this.attempts = attempts;
        }

        public long getCurrentOffset()
        {
            return currentOffset;
        }

        public void setCurrentOffset(long currentOffset)
        {
            this.currentOffset = currentOffset;
        }

        public Boolean getRangeRequestSupported()
        {
            return rangeRequestSupported;
        }

        public void setRangeRequestSupported(Boolean rangeRequestSupported)
        {
            this.rangeRequestSupported = rangeRequestSupported;
        }

        @Override
        public void close()
            throws IOException
        {
            try
            {
                try
                {
                    client.close();
                }
                catch (Exception e)
                {
                    throw new IOException(e);
                }
            } finally
            {
                artifactCopyContext.remove();
            }
        }

    }

    @FunctionalInterface
    public interface InputStreamRead
    {

        long read()
            throws IOException;
    }

}
