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
package com.folib.utils.directory;

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

