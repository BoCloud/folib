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
package com.folib.io;

import org.springframework.transaction.TransactionStatus;

import java.nio.file.Path;
import java.util.concurrent.locks.Lock;

public class RepositoryStreamContext {

    private Path path;

    private Lock lock;

    private boolean opened;

    private TransactionStatus transaction;

    private boolean artifactExists;

    private Boolean isLocked;

    public Path getPath() {
        return path;
    }

    public void setPath(Path path) {
        this.path = path;
    }

    public Lock getLock() {
        return lock;
    }

    public void setLock(Lock lock) {
        this.lock = lock;
    }

    public boolean isOpened() {
        return opened;
    }

    public void setOpened(boolean opened) {
        this.opened = opened;
    }

    public TransactionStatus getTransaction() {
        return transaction;
    }

    public void setTransaction(TransactionStatus transaction) {
        this.transaction = transaction;
    }

    public boolean getArtifactExists() {
        return artifactExists;
    }

    public void setArtifactExists(boolean artifactExists) {
        this.artifactExists = artifactExists;
    }

    public Boolean getLocked() {
        return isLocked;
    }

    public void setLocked(Boolean locked) {
        isLocked = locked;
    }
}
