package com.veadan.folib.utils.directory;

import java.util.concurrent.atomic.AtomicLong;

/**
 * @author leipenghui
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

    /**
     * 回收站文件数量
     */
    private final AtomicLong trashFilesCount = new AtomicLong();

    /**
     * 回收站目录数量
     */
    private final AtomicLong trashDirectoriesCount = new AtomicLong();

    /**
     * 回收站文件大小
     */
    private final AtomicLong trashTotalFilesSize = new AtomicLong();

    /**
     * 回收站制品数量
     */
    private final AtomicLong trashArtifactsCount = new AtomicLong();

    /**
     * 回收站制品大小
     */
    private final AtomicLong trashTotalArtifactsSize = new AtomicLong();

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

    public long getTrashFilesCount() {
        return trashFilesCount.get();
    }

    public void setTrashFilesCount(long trashFilesCount) {
        this.trashFilesCount.set(trashFilesCount);
    }

    public long getTrashDirectoriesCount() {
        return trashDirectoriesCount.get();
    }

    public void setTrashDirectoriesCount(long trashDirectoriesCount) {
        this.trashDirectoriesCount.set(trashDirectoriesCount);
    }

    public long getTrashTotalFilesSize() {
        return trashTotalFilesSize.get();
    }

    public void setTrashTotalFilesSize(long trashTotalFilesSize) {
        this.trashTotalFilesSize.set(trashTotalFilesSize);
    }

    public long getTrashArtifactsCount() {
        return trashArtifactsCount.get();
    }

    public void setTrashArtifactsCount(long trashArtifactsCount) {
        this.trashArtifactsCount.set(trashArtifactsCount);
    }

    public long getTrashTotalArtifactsSize() {
        return trashTotalArtifactsSize.get();
    }

    public void setTrashTotalArtifactsSize(long trashTotalArtifactsSize) {
        this.trashTotalArtifactsSize.set(trashTotalArtifactsSize);
    }

    public void incrementTrashFilesCount() {
        this.trashFilesCount.incrementAndGet();
    }

    public void incrementTrashDirectoriesCount() {
        this.trashDirectoriesCount.incrementAndGet();
    }

    public void addToTrashFilesSize(long size) {
        this.trashTotalFilesSize.addAndGet(size);
    }

    public void incrementTrashArtifactsCount() {
        this.trashArtifactsCount.incrementAndGet();
    }

    public void addToTrashArtifactsSize(long size) {
        this.trashTotalArtifactsSize.addAndGet(size);
    }

}

