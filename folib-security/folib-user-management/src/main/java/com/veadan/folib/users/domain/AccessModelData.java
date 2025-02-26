package com.veadan.folib.users.domain;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.users.dto.*;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
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
        return getPathAuthorities(url, getStorageAuthorities(), false);
    }

    @Override
    public Set<Privileges> getPathAuthorities(String url, boolean enableSplitPath) {
        return getPathAuthorities(url, getStorageAuthorities(), enableSplitPath);
    }

    @Override
    public Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths, boolean enableSplitPath) {
        return getPathAuthorities(storageId, repositoryId, paths, getStorageAuthorities(), enableSplitPath);
    }

    @Override
    public Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths) {
        return getPathAuthorities(storageId, repositoryId, paths, getStorageAuthorities(), false);
    }

    public static Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths, Set<? extends StoragePrivileges> storages, boolean enableSplitPath) {
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
                for (String itemPath : paths) {
                    for (PathPrivileges pathPrivilege : repository.getPathPrivileges()) {
                        String normalizedPath = StringUtils.chomp(pathPrivilege.getPath(), "/");
                        List<String> pathKeyList = Lists.newArrayList();
                        pathKeyList.add(normalizedPath);
                        if (enableSplitPath) {
                            pathKeyList = splitPath(itemPath, storage.getStorageId(), repository.getRepositoryId(), normalizedPath);
                        }
                        for (String path : pathKeyList) {
                            String pathKeyPattern = path;
                            if (!normalizedPath.contains("*") && !normalizedPath.contains("?")) {
                                pathKeyPattern = path + end;
                            }
                            if (!itemPath.startsWith(path) && !itemPath.matches(pathKeyPattern)) {
                                continue;
                            }
                            boolean flag = itemPath.matches(pathKeyPattern) || itemPath.equals(path) || pathPrivilege.isWildcard();
                            if (flag) {
                                privileges.addAll(pathPrivilege.getPrivileges());
                            }
                        }
                    }
                }
            }
        }
        return privileges;
    }

    public static Set<Privileges> getPathAuthorities(String url, Set<? extends StoragePrivileges> storages, boolean enableSplitPath) {
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
            String artifactoryKey = "/artifactory/" + storage.getStorageId() + separator;
            if (!normalizedUrl.startsWith(artifactoryKey) && !normalizedUrl.startsWith(storageKey) && !normalizedUrl.startsWith(dockerKey) && !normalizedUrl.startsWith(storageBrowseKey) && !normalizedUrl.startsWith(storageConfigKey)) {
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
                String artifactoryRepositoryKey = artifactoryKey + repository.getRepositoryId() + separator;
                if (!normalizedUrl.startsWith(artifactoryRepositoryKey) && !normalizedUrl.startsWith(repositoryKey) && !normalizedUrl.startsWith(repositoryDockerKey) && !normalizedUrl.startsWith(repositoryBrowseKey) && !normalizedUrl.startsWith(repositoryConfigKey)) {
                    continue;
                }
                privileges.addAll(repository.getRepositoryPrivileges());
                for (PathPrivileges pathPrivilege : repository.getPathPrivileges()) {
                    if (normalizedUrl.equals(artifactoryRepositoryKey) || normalizedUrl.equals(repositoryKey) || normalizedUrl.equals(repositoryBrowseKey)) {
                        privileges.addAll(pathPrivilege.getPrivileges());
                        continue;
                    }
                    String normalizedPath = StringUtils.chomp(pathPrivilege.getPath(), "/");
                    List<String> pathKeyList = Lists.newArrayList();
                    pathKeyList.add(normalizedPath);
                    if (enableSplitPath) {
                        pathKeyList = splitPath(normalizedUrl, storage.getStorageId(), repository.getRepositoryId(), normalizedPath);
                    }
                    for (String path : pathKeyList) {
                        String pathKey = StringUtils.removeEnd(repositoryKey, GlobalConstants.SEPARATOR) + GlobalConstants.SEPARATOR + path;
                        String artifactoryPathKey = StringUtils.removeEnd(artifactoryRepositoryKey, GlobalConstants.SEPARATOR) + GlobalConstants.SEPARATOR + path;
                        String pathBrowseKey = storageBrowseKey + repository.getRepositoryId() + GlobalConstants.SEPARATOR + path;
                        String pathKeyPattern = pathKey, pathBrowseKeyPattern = pathBrowseKey, artifactoryPathKeyPattern = artifactoryPathKey;
                        if (!normalizedPath.contains("*") && !normalizedPath.contains("?")) {
                            pathKeyPattern = pathKey + end;
                            artifactoryPathKeyPattern = artifactoryPathKey + end;
                            pathBrowseKeyPattern = pathBrowseKey + end;
                        }
                        if (!normalizedUrl.startsWith(artifactoryPathKey) && !normalizedUrl.startsWith(pathKey) && !normalizedUrl.startsWith(pathBrowseKey) && !normalizedUrl.matches(artifactoryPathKeyPattern) && !normalizedUrl.matches(pathKeyPattern) && !normalizedUrl.matches(pathBrowseKeyPattern)) {
                            continue;
                        }
                        boolean flag = normalizedUrl.matches(artifactoryPathKeyPattern) || normalizedUrl.matches(pathKeyPattern) || normalizedUrl.matches(pathBrowseKeyPattern) || normalizedUrl.equals(artifactoryPathKey) || normalizedUrl.equals(pathKey) || normalizedUrl.equals(pathBrowseKey) || pathPrivilege.isWildcard();
                        if (flag) {
                            privileges.addAll(pathPrivilege.getPrivileges());
                        }
                    }
                }
            }
        }
        return privileges;
    }

    public static List<String> splitPath(String normalizedUrl, String storageId, String repositoryId, String path) {
        String storageAndRepository = String.format("/%s/%s/", storageId, repositoryId);
        int level = normalizedUrl.substring(normalizedUrl.indexOf(storageAndRepository) + storageAndRepository.length()).split("/").length;
        List<String> result = Lists.newArrayList();
        String[] parts = path.split("/");
        StringBuilder currentPath = new StringBuilder();
        String appendPath = "";
        for (String part : parts) {
            if (currentPath.length() > 0) {
                currentPath.append("/");
            }
            currentPath.append(part);
            appendPath = currentPath.toString();
            if (appendPath.split("/").length == level) {
                result.add(StringUtils.removeStart(currentPath.toString(), GlobalConstants.SEPARATOR));
            }
        }
        String p = StringUtils.removeStart(path, GlobalConstants.SEPARATOR);
        if (!result.contains(p)) {
            result.add(p);
        }
        return result;
    }

}
