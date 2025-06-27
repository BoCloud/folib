package com.veadan.folib.components.jfrogArtifactSync;

import lombok.Data;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author veadan
 * @since 2024-12-31 13:12
 */
@Data
public class DownloadStats {
    private final int totalArtifacts;
    private final AtomicInteger completedArtifacts = new AtomicInteger(0);
    private final AtomicInteger failedArtifacts = new AtomicInteger(0);
    private final Map<String, String> failureReasons = new ConcurrentHashMap<>();

    public void incrementCompleted() {
        completedArtifacts.incrementAndGet();
    }

    public void recordFailure(String artifactPath, String reason) {
        failedArtifacts.incrementAndGet();
        failureReasons.put(artifactPath, reason);
    }

    public String getProgress() {
        return String.format("Progress: %d/%d (Failed: %d)",
                completedArtifacts.get(), totalArtifacts,
                failedArtifacts.get());
    }
}
