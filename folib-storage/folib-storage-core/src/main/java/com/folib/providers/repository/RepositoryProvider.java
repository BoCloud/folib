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
package com.folib.providers.repository;

import com.folib.data.criteria.Paginator;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryDto;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.security.NoSuchAlgorithmException;
import java.util.List;

import org.springframework.transaction.annotation.Transactional;

/**
 * This interface provide functionality to operate with artifact Paths.
 * Implementation depends of {@link Repository} type which can be: Hosted, Group
 * or Proxy.
 *
 * TODO: should be replaced with `RepositoryFileSystemProvider`
 *
 * @author Veadan
 */
public interface RepositoryProvider
{

    /**
     * Return {@link RepositoryDto} type alias.
     *
     * @return
     */
    String getAlias();

    /**
     * Return {@link InputStream} to read Artifact content.
     *
     * @param path
     * @return
     * @throws IOException
     */
    InputStream getInputStream(Path path) throws IOException;

    /**
     * Return {@link InputStream} to read Artifact content.
     *
     * @param path
     * @return
     * @throws IOException
     */
    InputStream getStoreIndexInputStream(Path path) throws IOException;

    /**
     * Return {@link OutputStream} to write Artifact content.
     *
     * @param path
     * @return
     * @throws IOException
     * @throws NoSuchAlgorithmException
     */
    OutputStream getOutputStream(Path path)
            throws IOException, NoSuchAlgorithmException;

    /**
     * Searches Artifact Paths. For Group Repositories result will be group
     * member Paths.
     *
     * @param storageId
     * @param repositoryId
     * @param predicate
     * @param paginator
     * @return
     */
    @Transactional(readOnly = true)
    List<Path> search(String storageId,
                      String repositoryId,
                      RepositorySearchRequest predicate,
                      Paginator paginator);

    /**
     * Counts Artifacts. For Group repositories result will be distinct within
     * group members.
     *
     * @param storageId
     * @param repositoryId
     * @param predicate
     * @return
     */
    @Transactional(readOnly = true)
    Long count(String storageId,
               String repositoryId,
               RepositorySearchRequest predicate);

    /**
     * Fetch Artifact Path from target repository.
     * For Group repository it will resolve Path from underlying group member.
     * For Proxy repository it will try to download remote Artifact if it's not cached.
     * Return  <code>null<code> if there is no such Path in target repository.
     *
     * To resolve target path you should use {@link RepositoryPathResolver}
     *
     * @param repositoryPath
     * @return
     * @throws IOException
     */
    Path fetchPath(Path repositoryPath)
        throws IOException;
}
