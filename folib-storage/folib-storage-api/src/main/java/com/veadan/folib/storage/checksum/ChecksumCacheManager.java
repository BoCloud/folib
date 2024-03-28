package com.veadan.folib.storage.checksum;

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
 * @author mtodorov
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
                .ifPresent(ac -> logger.info("Removed [{}] artifact checksum value [{}] from cache current size [{}].",
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
