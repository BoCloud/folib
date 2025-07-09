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
package com.folib.storage.checksum;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * All artifacts should pass through here.
 * Any deployed file which doesn't end in a checksum format (md5, sha1, gpg)
 * should add a cachedChecksum. When an actual checksum is deployed,
 * (which is right after the artifact or metadata file has been deployed),
 * the cache should be queried for a match.
 * <p>
 * If:
 * - a match is found and matches, remove it from the cache. (If the checksums set
 * is empty, remove the respective Checksum from the cachedChecksums).
 * - a match is found, but does not match, trigger an event and log this, then remove
 * the checksum from the cache. (If the checksums set is empty, remove the respective
 * Checksum from the cachedChecksums).
 * - a checksum is not claimed within cachedChecksumLifetime, trigger an event and log
 * this, then remove the checksum from the cache. (If this checksums set is empty,
 * remove the respective Checksum from the cachedChecksums).
 *
 * @author veadan
 */
public class ChecksumCacheManager
{

    private static Logger logger = LoggerFactory.getLogger(ChecksumCacheManager.class);

    /**
     * Key:     Artifact path
     * Value:   Artifact checksum.
     */
    private Map<String, ArtifactChecksum> cachedChecksums = new LinkedHashMap<>();

    /**
     * Specifies how long to keep the cached checksums.
     * <p>
     * The default is five minutes.
     */
    private long cachedChecksumLifetime = 5 * 60000;

    /**
     * Specifies at what interval to check if the checksums have expired.
     * The default is to check once every minute.
     */
    private long cachedChecksumExpiredCheckInterval = 60000L;


    public ChecksumCacheManager()
    {
        startMonitor();
    }

    public boolean containsArtifactPath(String artifactPath)
    {
        final boolean containsChecksum = cachedChecksums.containsKey(artifactPath);
        if (containsChecksum)
        {
            logger.debug("Cache contains artifact path '{}'.", artifactPath);
        }

        return containsChecksum;
    }

    public String getArtifactChecksum(String artifactBasePath,
                                      String algorithm)
    {
        if (!cachedChecksums.containsKey(artifactBasePath))
        {
            return null;
        }

        final ArtifactChecksum artifactChecksum = getArtifactChecksum(artifactBasePath);
        final String checksum = artifactChecksum.getChecksum(algorithm);
        if (checksum != null)
        {
            logger.debug("Found checksum '{}' [{}] for '{}' in cache.", checksum, algorithm, artifactBasePath);
        }

        return checksum;
    }

    public ArtifactChecksum getArtifactChecksum(String artifactBasePath)
    {
        return cachedChecksums.get(artifactBasePath);
    }

    public boolean validateChecksum(String artifactPath,
                                    String algorithm,
                                    String checksum)
    {
        return getArtifactChecksum(artifactPath, algorithm).equals(checksum);
    }

    public synchronized void addArtifactChecksum(String artifactBasePath,
                                                 String algorithm,
                                                 String checksum)
    {
        logger.debug("Adding checksum '{}' [{}] for '{}' in cache.", checksum, algorithm, artifactBasePath);

        if (cachedChecksums.containsKey(artifactBasePath))
        {
            final ArtifactChecksum artifactChecksum = getArtifactChecksum(artifactBasePath);
            artifactChecksum.addChecksum(algorithm, checksum);
        }
        else
        {
            ArtifactChecksum artifactChecksum = new ArtifactChecksum();
            artifactChecksum.addChecksum(algorithm, checksum);

            cachedChecksums.put(artifactBasePath, artifactChecksum);
        }
    }

    public synchronized void removeArtifactChecksum(String artifactBasePath,
                                                    String algorithm)
    {
        Optional.ofNullable(getArtifactChecksum(artifactBasePath)).map(ac -> {
            logger.debug(ac.removeChecksum(algorithm)
                           .map(c -> String.format("Removed [%s] artifact checksum value [%s] from cache.",
                                                   artifactBasePath,
                                                   c))
                           .orElseGet(() -> String.format("Checksum algorithm [%s] not found for [%s] in cache.",
                                                          algorithm,
                                                          artifactBasePath)));
            return ac;
        }).filter(ac -> ac.getChecksums().isEmpty()).ifPresent(ac -> cachedChecksums.values().remove(ac));
    }

    public synchronized void removeArtifactChecksum(String artifactBasePath)
    {
        Optional.ofNullable(cachedChecksums.remove(artifactBasePath))
                .ifPresent(ac -> logger.debug("Removed [{}] artifact checksum value [{}] from cache current size [{}].",
                                              artifactBasePath, ac, cachedChecksums.size()));
    }

    public synchronized void removeExpiredChecksums()
    {
        for (String checksum : getExpiredChecksums())
        {
            removeArtifactChecksum(checksum);
        }
    }

    private Set<String> getExpiredChecksums()
    {
        Set<String> expiredChecksums = new LinkedHashSet<>();

        for (Map.Entry<String, ArtifactChecksum> artifactChecksumEntry : cachedChecksums.entrySet())
        {
            ArtifactChecksum checksum = artifactChecksumEntry.getValue();

            if (System.currentTimeMillis() - checksum.getLastAccessed() > cachedChecksumLifetime)
            {
                expiredChecksums.add(artifactChecksumEntry.getKey());
            }
        }

        return expiredChecksums;
    }

    public long getCachedChecksumLifetime()
    {
        return cachedChecksumLifetime;
    }

    public void setCachedChecksumLifetime(long cachedChecksumLifetime)
    {
        this.cachedChecksumLifetime = cachedChecksumLifetime;
    }

    public long getCachedChecksumExpiredCheckInterval()
    {
        return cachedChecksumExpiredCheckInterval;
    }

    public void setCachedChecksumExpiredCheckInterval(long cachedChecksumExpiredCheckInterval)
    {
        this.cachedChecksumExpiredCheckInterval = cachedChecksumExpiredCheckInterval;
    }

    public long getSize()
    {
        return cachedChecksums.size();
    }

    public void startMonitor()
    {
        new CachedChecksumExpirer();
    }

    private class CachedChecksumExpirer
            extends Thread
    {

        private CachedChecksumExpirer()
        {
            start();
        }

        @Override
        public void run()
        {
            try
            {
                //noinspection InfiniteLoopStatement
                while (true)
                {
                    Thread.sleep(getCachedChecksumExpiredCheckInterval());
                    removeExpiredChecksums();
                }
            }
            catch (InterruptedException e)
            {
                logger.error(e.getMessage(), e);
            }
        }
    }

}
