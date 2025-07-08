package com.folib.converters;

import com.folib.domain.AccessModel;
import com.folib.domain.RepositoryAccessModel;
import com.folib.users.domain.Privileges;
import com.folib.users.dto.AccessModelDto;
import com.folib.users.dto.PathPrivilegesDto;
import com.folib.users.dto.RepositoryPrivilegesDto;
import com.folib.users.dto.StoragePrivilegesDto;
import org.apache.commons.lang3.StringUtils;

import java.util.Collection;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
public class AccessModelToUserAccessModelDtoConverter {

    public static AccessModelDto convert(AccessModel accessModel) {
        if (accessModel == null) {
            return null;
        }
        AccessModelDto userAccessModelDto = new AccessModelDto();
        accessModel.getApiAccess()
                .stream()
                .map(p -> Privileges.valueOf(p))
                .forEach(p -> userAccessModelDto.getApiAuthorities().add(p));
        for (RepositoryAccessModel repositoryAccessModel : accessModel.getRepositoriesAccess()) {
            StoragePrivilegesDto storage = userAccessModelDto.getStorageAuthorities(repositoryAccessModel.getStorageId())
                    .orElseGet(
                            () ->
                            {
                                StoragePrivilegesDto userStorageDto = new StoragePrivilegesDto();
                                userStorageDto.setStorageId(
                                        repositoryAccessModel.getStorageId());
                                if (StringUtils.isBlank(repositoryAccessModel.getRepositoryId())) {
                                    userStorageDto.getStoragePrivileges().addAll(pullPrivileges(repositoryAccessModel));
                                }
                                userAccessModelDto.getStorageAuthorities().add(userStorageDto);
                                return userStorageDto;
                            });
            if (StringUtils.isBlank(repositoryAccessModel.getRepositoryId())) {
                continue;
            }
            RepositoryPrivilegesDto repository = storage.getRepositoryPrivileges(repositoryAccessModel.getRepositoryId())
                    .orElseGet(
                            () ->
                            {
                                RepositoryPrivilegesDto userRepositoryDto = new RepositoryPrivilegesDto();
                                userRepositoryDto.setRepositoryId(
                                        repositoryAccessModel.getRepositoryId());
                                storage.getRepositoryPrivileges().add(userRepositoryDto);
                                return userRepositoryDto;
                            });
            if (StringUtils.isBlank(repositoryAccessModel.getPath())) {
                repository.getRepositoryPrivileges().addAll(pullPrivileges(repositoryAccessModel));
                continue;
            }

            PathPrivilegesDto pathPrivileges = repository.getPathPrivilege(repositoryAccessModel.getPath(),
                    repositoryAccessModel.isWildcard())
                    .orElseGet(
                            () -> {
                                PathPrivilegesDto pathPrivilegesDto = new PathPrivilegesDto();
                                pathPrivilegesDto.setPath(
                                        repositoryAccessModel.getPath());
                                pathPrivilegesDto.setWildcard(
                                        repositoryAccessModel.isWildcard());
                                repository.getPathPrivileges()
                                        .add(
                                                pathPrivilegesDto);
                                return pathPrivilegesDto;
                            });
            pathPrivileges.getPrivileges().addAll(pullPrivileges(repositoryAccessModel));
        }
        return userAccessModelDto;
    }

    private static Collection<Privileges> pullPrivileges(final RepositoryAccessModel repositoryAccessModel) {
        return repositoryAccessModel.getPrivileges()
                .stream()
                .map(p -> Privileges.valueOf(p))
                .collect(Collectors.toSet());
    }
}
