package com.veadan.folib.users.domain;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.users.dto.*;
import org.apache.commons.lang.StringUtils;

import javax.annotation.concurrent.Immutable;
import java.io.File;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static java.util.stream.Collectors.toSet;

/**
 * @author veadan
 */
@Immutable
public class AccessModelData
        implements Serializable, AccessModel {

    private final Set<Privileges> apiAuthorities;

    private final Set<StoragePrivilegesData> storageAuthorities;


    public AccessModelData(AccessModelDto delegate) {
        this.storageAuthorities = immuteStorages(delegate.getStorageAuthorities());
        this.apiAuthorities = ImmutableSet.copyOf(delegate.getApiAuthorities());
    }

    private Set<StoragePrivilegesData> immuteStorages(final Set<StoragePrivilegesDto> source) {
        return source != null ?
                ImmutableSet.copyOf(
                        source.stream().map(StoragePrivilegesData::new).collect(toSet())) :
                Collections.emptySet();
    }

    @Override
    public Set<Privileges> getApiAuthorities() {
        return apiAuthorities;
    }

    public Set<StoragePrivilegesData> getStorageAuthorities() {
        return storageAuthorities;
    }

    @Override
    public Set<Privileges> getPathAuthorities(String url) {
        return getPathAuthorities(url, getStorageAuthorities());
    }

    @Override
    public Set<Privileges> getPathAuthorities(String storageId, String repositoryId) {
        return getPathAuthorities(storageId, repositoryId, getStorageAuthorities());
    }

    public static Set<Privileges> getPathAuthorities(String storageId, String repositoryId, Set<? extends StoragePrivileges> storages) {
        if (StringUtils.isBlank(storageId)) {
            return Collections.emptySet();
        }
        Set<Privileges> privileges = new HashSet<>();
        for (final StoragePrivileges storage : storages) {
            if (!storage.getStorageId().equals(storageId)) {
                continue;
            }
            privileges.addAll(storage.getStoragePrivileges());
            if (StringUtils.isBlank(repositoryId)) {
                continue;
            }
            for (RepositoryPrivileges repository : storage.getRepositoryPrivileges()) {
                if (!repository.getRepositoryId().equals(repositoryId)) {
                    continue;
                }
                privileges.addAll(repository.getRepositoryPrivileges());
            }
        }
        return privileges;
    }

    public static Set<Privileges> getPathAuthorities(String url, Set<? extends StoragePrivileges> storages) {
        String normalizedUrl = StringUtils.chomp(url, "/");
        boolean isEnd = false;
        String separator = "";
        Set<Privileges> privileges = new HashSet<>();
        for (final StoragePrivileges storage : storages) {
            isEnd = normalizedUrl.endsWith(storage.getStorageId());
            separator = "";
            if (!isEnd) {
                separator = "/";
            }
            String storageKey = "/storages/" + storage.getStorageId() + separator;
            String dockerKey = "/v2/" + storage.getStorageId() + separator;
            String storageBrowseKey = "/api/browse/" + storage.getStorageId() + separator;
            String storageConfigKey = "/api/configuration/folib/storages/" + storage.getStorageId() + separator;
            if (!normalizedUrl.startsWith(storageKey) && !normalizedUrl.startsWith(dockerKey) && !normalizedUrl.startsWith(storageBrowseKey) && !normalizedUrl.startsWith(storageConfigKey)) {
                continue;
            }
            privileges.addAll(storage.getStoragePrivileges());
            for (RepositoryPrivileges repository : storage.getRepositoryPrivileges()) {
                String repositoryKey = storageKey + repository.getRepositoryId() + "/";
                String repositoryDockerKey = dockerKey + repository.getRepositoryId() + "/";
                String repositoryBrowseKey = storageBrowseKey + repository.getRepositoryId() + "/";
                String repositoryConfigKey = storageConfigKey + repository.getRepositoryId() + "/";
                if (!normalizedUrl.startsWith(repositoryKey) && !normalizedUrl.startsWith(repositoryDockerKey) && !normalizedUrl.startsWith(repositoryBrowseKey) && !normalizedUrl.startsWith(repositoryConfigKey)) {
                    continue;
                }
                privileges.addAll(repository.getRepositoryPrivileges());
                for (PathPrivileges pathPrivilege : repository.getPathPrivileges()) {
                    String normalizedPath = StringUtils.chomp(pathPrivilege.getPath(), "/");
                    String pathKey = repositoryKey + normalizedPath;

                    if (!normalizedUrl.startsWith(pathKey)) {
                        continue;
                    }
                    if (normalizedUrl.equals(pathKey) || pathPrivilege.isWildcard()) {
                        privileges.addAll(pathPrivilege.getPrivileges());
                    }
                }
            }
        }
        return privileges;
    }

}
