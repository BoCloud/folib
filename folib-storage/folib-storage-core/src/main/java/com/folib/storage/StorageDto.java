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
package com.folib.storage;

import java.io.Serializable;
import java.util.*;

import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryDto;
import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonRootName;

/**
 * @author veadan
 * @author Veadan
 */
@JsonRootName("storage")
public class StorageDto
        implements Serializable, Storage
{
    private String id;
    
    private String basedir;

    private String admin;

    private String storageProvider;

    private Set<String> users = new LinkedHashSet<>();
    private Set<String> repositoryUsers = new LinkedHashSet<>();

    private Map<String, RepositoryDto> repositories = new LinkedHashMap<>();

    public StorageDto()
    {
    }

    @Override
    public Set<String> getUsers() {
        return users;
    }

    public void setUsers(Set<String> users) {
        this.users = users;
    }

    @JsonCreator
    public StorageDto(@JsonProperty(value = "id", required = true) String id,@JsonProperty(value = "users", required = false) Set<String> users)
    {
        this.id = id;
        this.users=users;
    }

    @Override
    public boolean containsRepository(String repository)
    {
        return getRepositories().containsKey(repository);
    }

    @Override
    public String getId()
    {
        return id;
    }

    public void setId(String id)
    {
        this.id = id;
    }

    @Override
    public String getBasedir()
    {
        return basedir;
    }

    public void setBasedir(String basedir)
    {
        this.basedir = basedir;
    }

    @Override
    public String getAdmin() {
        return admin;
    }

    public void setAdmin(String admin) {
        this.admin = admin;
    }

    @Override
    public String getStorageProvider() {
        return storageProvider;
    }

    public void setStorageProvider(String storageProvider) {
        this.storageProvider = storageProvider;
    }

    @Override
    public Map<String, ? extends Repository> getRepositories()
    {
        return repositories;
    }

    public void setRepositories(Map<String, RepositoryDto> repositories)
    {
        this.repositories = repositories;
    }

    public void addRepository(RepositoryDto repository)
    {
        repositories.put(repository.getId(), repository);
    }

    @Override
    public RepositoryDto getRepository(String repositoryId)
    {
        return repositories.get(repositoryId);
    }

    public void removeRepository(String repositoryId)
    {
        repositories.remove(repositoryId);
    }

    public boolean hasRepositories()
    {
        return !CollectionUtils.isEmpty(repositories);
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("Storage{");
        sb.append("\n\t\tid='")
          .append(id)
          .append('\'');
        sb.append(", \n\t\tbasedir='").append(basedir).append('\'');
        sb.append(", \n\t\trepositories=").append(repositories);
        sb.append('}');
        return sb.toString();
    }

    public Set<String> getRepositoryUsers() {
        return repositoryUsers;
    }

    public void setRepositoryUsers(Set<String> repositoryUsers) {
        this.repositoryUsers = repositoryUsers;
    }
}
