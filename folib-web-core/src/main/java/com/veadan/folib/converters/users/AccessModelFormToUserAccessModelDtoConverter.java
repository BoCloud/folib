package com.veadan.folib.converters.users;

import java.util.Collection;
import java.util.stream.Collectors;

import com.veadan.folib.forms.users.AccessModelForm;
import com.veadan.folib.forms.users.RepositoryAccessModelForm;
import org.apache.commons.lang3.StringUtils;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.dto.PathPrivilegesDto;
import com.veadan.folib.users.dto.RepositoryPrivilegesDto;
import com.veadan.folib.users.dto.StoragePrivilegesDto;
import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 * @author veadan
 */
public enum AccessModelFormToUserAccessModelDtoConverter
        implements Converter<AccessModelForm, com.veadan.folib.users.dto.AccessModelDto>
{

    INSTANCE;

    @Override
    public com.veadan.folib.users.dto.AccessModelDto convert(AccessModelForm accessModelForm)
    {
        if (accessModelForm == null)
        {
            return null;
        }
        
        com.veadan.folib.users.dto.AccessModelDto userAccessModelDto = new com.veadan.folib.users.dto.AccessModelDto();
        accessModelForm.getApiAccess()
                       .stream()
                       .map(p -> Privileges.valueOf(p))
                       .forEach(p -> userAccessModelDto.getApiAuthorities().add(p));
        
        for (RepositoryAccessModelForm repositoryAccess : accessModelForm.getRepositoriesAccess())
        {
            StoragePrivilegesDto storage = userAccessModelDto.getStorageAuthorities(repositoryAccess.getStorageId())
                                                       .orElseGet(
                                                               () ->
                                                               {
                                                                   StoragePrivilegesDto userStorageDto = new StoragePrivilegesDto();
                                                                   userStorageDto.setStorageId(
                                                                           repositoryAccess.getStorageId());
                                                                   if (StringUtils.isBlank(repositoryAccess.getRepositoryId())) {
                                                                       userStorageDto.getStoragePrivileges().addAll(pullPrivileges(repositoryAccess));
                                                                   }
                                                                   userAccessModelDto.getStorageAuthorities().add(userStorageDto);
                                                                   return userStorageDto;
                                                               });

            if (StringUtils.isBlank(repositoryAccess.getRepositoryId())) {
                continue;
            }
            RepositoryPrivilegesDto repository = storage.getRepositoryPrivileges(repositoryAccess.getRepositoryId())
                                                  .orElseGet(
                                                          () ->
                                                          {
                                                              RepositoryPrivilegesDto userRepositoryDto = new RepositoryPrivilegesDto();
                                                              userRepositoryDto.setRepositoryId(
                                                                      repositoryAccess.getRepositoryId());
                                                              storage.getRepositoryPrivileges().add(userRepositoryDto);
                                                              return userRepositoryDto;
                                                          });

            if (StringUtils.isBlank(repositoryAccess.getPath()))
            {
                repository.getRepositoryPrivileges().addAll(pullPrivileges(repositoryAccess));
                continue;
            }

            PathPrivilegesDto pathPrivileges = repository.getPathPrivilege(repositoryAccess.getPath(),
                                                                           repositoryAccess.isWildcard())
                                                         .orElseGet(
                                                                    () -> {
                                                                        PathPrivilegesDto pathPrivilegesDto = new PathPrivilegesDto();
                                                                        pathPrivilegesDto.setPath(
                                                                                                  repositoryAccess.getPath());
                                                                        pathPrivilegesDto.setWildcard(
                                                                                                      repositoryAccess.isWildcard());
                                                                        repository.getPathPrivileges()
                                                                                  .add(
                                                                                       pathPrivilegesDto);
                                                                        return pathPrivilegesDto;
                                                                    });
            pathPrivileges.getPrivileges().addAll(pullPrivileges(repositoryAccess));

        }
        return userAccessModelDto;
    }

    private Collection<Privileges> pullPrivileges(final RepositoryAccessModelForm repositoryAccess)
    {
        return repositoryAccess.getPrivileges()
                               .stream()
                               .map(p -> Privileges.valueOf(p))
                               .collect(Collectors.toSet());
    }
}
