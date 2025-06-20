package com.veadan.folib.dto.configuration;

import com.veadan.folib.validation.configuration.UniqueStorage;

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
public class StorageDto {

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
     * 存储配额
     */
    private Long storageMaxSize;

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
    private List<RepositoryDto> repositories;

    public boolean isSyncEnabled() {
        return syncEnabled;
    }

    public void setSyncEnabled(boolean syncEnabled) {
        this.syncEnabled = syncEnabled;
    }

    /**是否同步存储空间到其他节点*/
    private boolean syncEnabled;

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

    public List<RepositoryDto> getRepositories() {
        return repositories;
    }

    public void setRepositories(final List<RepositoryDto> repositories) {
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

    public Long getStorageMaxSize() {
        return storageMaxSize;
    }

    public void setStorageMaxSize(Long storageMaxSize) {
        this.storageMaxSize = storageMaxSize;
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
