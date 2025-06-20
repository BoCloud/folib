package com.veadan.folib.converters.configuration;

import com.veadan.folib.dto.configuration.RepositoryPermissionDto;
import com.veadan.folib.storage.repository.RepositoryPermissionUserDto;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.core.convert.converter.Converter;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 */
public enum RepositoryPermissionFormConverter
        implements Converter<RepositoryPermissionDto, com.veadan.folib.storage.repository.RepositoryPermissionDto> {
    INSTANCE;

    @Override
    public com.veadan.folib.storage.repository.RepositoryPermissionDto convert(final RepositoryPermissionDto source) {
        com.veadan.folib.storage.repository.RepositoryPermissionDto result = new com.veadan.folib.storage.repository.RepositoryPermissionDto();
        result.setScope(source.getScope());
        result.setAllowAnonymous(source.isAllowAnonymous());
        if (CollectionUtils.isNotEmpty(source.getUserList())) {
            List<RepositoryPermissionUserDto> repositoryPermissionUserList = source.getUserList().stream().map(item -> {
                RepositoryPermissionUserDto repositoryPermissionUserDto = new RepositoryPermissionUserDto();
                BeanUtils.copyProperties(item, repositoryPermissionUserDto);
                return repositoryPermissionUserDto;
            }).collect(Collectors.toList());
            result.setUserList(repositoryPermissionUserList);
        }
        return result;
    }
}
