package com.veadan.folib.users.domain;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.users.dto.*;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;

import javax.annotation.concurrent.Immutable;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
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
    public Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths) {
        return getPathAuthorities(storageId, repositoryId, paths, getStorageAuthorities());
    }

    public static Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths, Set<? extends StoragePrivileges> storages) {
        if (StringUtils.isBlank(storageId)) {
            return Collections.emptySet();
        }
        Set<Privileges> privileges = new HashSet<>();
        String pattern = null, end = "/.*";
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
                if (CollectionUtils.isEmpty(paths)) {
                    for (PathPrivileges pathPrivilege : repository.getPathPrivileges()) {
                        privileges.addAll(pathPrivilege.getPrivileges());
                    }
                    continue;
                }
                for (String path : paths) {
                    for (PathPrivileges pathPrivilege : repository.getPathPrivileges()) {
                        pattern = pathPrivilege.getPath();
                        if (StringUtils.isBlank(pattern)) {
                            continue;
                        }
                        pattern = StringUtils.chomp(pattern, "/");
                        if (path.equals(pattern)) {
                            privileges.addAll(pathPrivilege.getPrivileges());
                            break;
                        }
                        if (!pattern.endsWith(end)) {
                            pattern = pattern + end;
                        }
                        if (path.matches(pattern)) {
                            privileges.addAll(pathPrivilege.getPrivileges());
                            break;
                        }
                    }
                }
            }
        }
        return privileges;
    }

    public static Set<Privileges> getPathAuthorities(String url, Set<? extends StoragePrivileges> storages) {
        String normalizedUrl = StringUtils.chomp(url, "/");
        boolean isEnd = false;
        String separator = "", end = "/.*";
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
                isEnd = normalizedUrl.endsWith(repository.getRepositoryId());
                separator = "";
                if (!isEnd) {
                    separator = "/";
                }
                String repositoryKey = storageKey + repository.getRepositoryId() + separator;
                String repositoryDockerKey = dockerKey + repository.getRepositoryId() + separator;
                String repositoryBrowseKey = storageBrowseKey + repository.getRepositoryId() + separator;
                String repositoryConfigKey = storageConfigKey + repository.getRepositoryId() + separator;
                if (!normalizedUrl.startsWith(repositoryKey) && !normalizedUrl.startsWith(repositoryDockerKey) && !normalizedUrl.startsWith(repositoryBrowseKey) && !normalizedUrl.startsWith(repositoryConfigKey)) {
                    continue;
                }
                privileges.addAll(repository.getRepositoryPrivileges());
                for (PathPrivileges pathPrivilege : repository.getPathPrivileges()) {
                    String normalizedPath = StringUtils.chomp(pathPrivilege.getPath(), "/");
                    String pathKey = repositoryKey + normalizedPath;
                    String pathBrowseKey = storageBrowseKey + repository.getRepositoryId() + separator + normalizedPath;
                    if (!normalizedUrl.startsWith(pathKey) && !normalizedUrl.startsWith(pathBrowseKey)) {
                        continue;
                    }
                    String pathKeyPattern = pathKey + end;
                    String pathBrowseKeyPattern = pathBrowseKey + end;
                    boolean flag = normalizedUrl.matches(pathKeyPattern) || normalizedUrl.matches(pathBrowseKeyPattern) || normalizedUrl.equals(pathKey) || normalizedUrl.equals(pathBrowseKey) || pathPrivilege.isWildcard();
                    if (flag) {
                        privileges.addAll(pathPrivilege.getPrivileges());
                    }
                }
            }
        }
        return privileges;
    }

}
