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
package com.folib.forms.configuration;

import com.folib.validation.configuration.UniqueStorage;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;
import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * @author veadan
 * @author Veadan
 */
public class StorageForm {

    @NotEmpty(message = "An id must be specified.")
    @UniqueStorage(groups = NewStorage.class, message = "The storage id already exists.")
    @Pattern(regexp = "[a-zA-Z0-9\\-\\_\\.]+")
    private String id;

    private String basedir;

    /**
     * 管理员
     */
    private String admin;

    /**
     * 存储类型 local、s3
     */
    private String storageProvider;


    /**
     * 普通用户
     */
    private Set<String> users = new LinkedHashSet<>();

    public Set<String> getUsers() {
        return users;
    }

    public void setUsers(Set<String> users) {
        this.users = users;
    }

    @Valid
    private List<RepositoryForm> repositories;

    public String getId() {
        return id;
    }

    public void setId(final String id) {
        this.id = id;
    }

    public String getBasedir() {
        return basedir;
    }

    public void setBasedir(final String basedir) {
        this.basedir = basedir;
    }

    public List<RepositoryForm> getRepositories() {
        return repositories;
    }

    public void setRepositories(final List<RepositoryForm> repositories) {
        this.repositories = repositories;
    }

    public String getAdmin() {
        return admin;
    }

    public void setAdmin(String admin) {
        this.admin = admin;
    }

    public String getStorageProvider() {
        return storageProvider;
    }

    public void setStorageProvider(String storageProvider) {
        this.storageProvider = storageProvider;
    }

    public interface NewStorage
            extends Serializable {
        // validation group marker interface for new storages.
    }

    public interface ExistingStorage
            extends Serializable {
        // validation group marker interface for existing storages.
    }

}
