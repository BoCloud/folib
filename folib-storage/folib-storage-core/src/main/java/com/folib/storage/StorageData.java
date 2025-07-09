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

import com.fasterxml.jackson.annotation.JsonView;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.collect.ImmutableSortedMap;
import com.folib.json.MapValuesJsonSerializer;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryData;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

import javax.annotation.concurrent.Immutable;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import static java.util.stream.Collectors.toMap;

/**
 * @author veadan
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
public class StorageData implements Storage {

    @JsonView(Views.ShortStorage.class)
    private String id;

    @JsonView(Views.ShortStorage.class)
    private String basedir;

    @JsonView(Views.ShortStorage.class)
    private String admin;

    @JsonView(Views.ShortStorage.class)
    private String storageProvider;

    @JsonView(Views.ShortStorage.class)
    private Long storageMaxSize;

    @JsonView(Views.ShortStorage.class)
    private Set<String> users;
    /**是否同步存储空间到其他节点*/
    @JsonView(Views.ShortStorage.class)
    private boolean syncEnabled;
    @JsonView(Views.ShortStorage.class)
    private Set<String> repositoryUsers;

    @JsonView(Views.LongStorage.class)
    @JsonSerialize(using = MapValuesJsonSerializer.class)
    @JsonDeserialize(using = RepositoryArrayToMapJsonDeserializer.class)
    private Map<String, ? extends Repository> repositories;

    StorageData() {

    }

    public StorageData(final Storage delegate) {
        this.id = delegate.getId();
        this.basedir = delegate.getBasedir();
        this.admin = delegate.getAdmin();
        this.storageProvider = delegate.getStorageProvider();
        this.users = delegate.getUsers();
        this.repositories = immuteRepositories(delegate.getRepositories());
        this.repositoryUsers = delegate.getRepositoryUsers();
    }

    private Map<String, ? extends Repository> immuteRepositories(final Map<String, ? extends Repository> source) {
        return source != null ? ImmutableSortedMap.copyOf(source.entrySet().stream().collect(
                toMap(Map.Entry::getKey, e -> new RepositoryData(e.getValue(), this)))) : Collections.emptyMap();
    }

    @Override
    public Repository getRepository(final String repositoryId) {
        return repositories.get(repositoryId);
    }

    @Override
    public boolean containsRepository(final String repository) {
        return repositories.containsKey(repository);
    }

    @Override
    public String getStorageProvider() {
        return storageProvider;
    }


    @Override
    public void setUsers(Set<String> users) {
        this.users = users;
    }

    @Override
    public void setAdmin(String username) {
        this.admin = username;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public String getAdmin() {
        return admin;
    }

    @Override
    public String getBasedir() {
        return basedir;
    }

    @Override
    public Set<String> getUsers() {
        return users;
    }

    @Override
    public Map<String, ? extends Repository> getRepositories() {
        return repositories;
    }


    @Override
    public Set<String> getRepositoryUsers() {
        return repositoryUsers;
    }

    @Override
    public void setRepositoryUsers(Set<String> repositoryUsers) {
        this.repositoryUsers = repositoryUsers;
    }

    public void setSyncEnabled(boolean syncEnabled) {
        this.syncEnabled = syncEnabled;
    }
}
