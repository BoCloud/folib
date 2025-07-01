package com.veadan.folib.utils.directory;

import java.util.concurrent.atomic.AtomicLong;

/**
 * @author veadan
 * @date 2024/7/18
 **/
public class Result {

    /**
     * 文件数量
     */
    private final AtomicLong filesCount = new AtomicLong();

    /**
     * 目录数量
     */
    private final AtomicLong directoriesCount = new AtomicLong();

    /**
     * 文件大小
     */
    private final AtomicLong totalFilesSize = new AtomicLong();

    /**
     * 制品数量
     */
    private final AtomicLong artifactsCount = new AtomicLong();

    /**
     * 制品大小
     */
    private final AtomicLong totalArtifactsSize = new AtomicLong();

    public long getFilesCount() {
        return filesCount.get();
    }

    public void setFilesCount(long filesCount) {
        this.filesCount.set(filesCount);
    }

    public long getDirectoriesCount() {
        return directoriesCount.get();
    }

    public void setDirectoriesCount(long directoriesCount) {
        this.directoriesCount.set(directoriesCount);
    }

    public long getTotalFilesSize() {
        return totalFilesSize.get();
    }

    public void setTotalFilesSize(long totalFilesSize) {
        this.totalFilesSize.set(totalFilesSize);
    }

    public long getArtifactsCount() {
        return artifactsCount.get();
    }

    public void setArtifactsCount(long artifactsCount) {
        this.artifactsCount.set(artifactsCount);
    }

    public long getTotalArtifactsSize() {
        return totalArtifactsSize.get();
    }

    public void setTotalArtifactsSize(long totalArtifactsSize) {
        this.totalArtifactsSize.set(totalArtifactsSize);
    }

    public void incrementFilesCount() {
        this.filesCount.incrementAndGet();
    }

    public void incrementDirectoriesCount() {
        this.directoriesCount.incrementAndGet();
    }

    public void addToFilesSize(long size) {
        this.totalFilesSize.addAndGet(size);
    }

    public void incrementArtifactsCount() {
        this.artifactsCount.incrementAndGet();
    }

    public void addToArtifactsSize(long size) {
        this.totalArtifactsSize.addAndGet(size);
    }

}

