package com.veadan.folib.converters.configuration;

import com.veadan.folib.forms.configuration.RepositoryPermissionForm;
import com.veadan.folib.storage.repository.RepositoryPermissionDto;
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
        implements Converter<RepositoryPermissionForm, RepositoryPermissionDto> {
    INSTANCE;

    @Override
    public RepositoryPermissionDto convert(final RepositoryPermissionForm source) {
        RepositoryPermissionDto result = new RepositoryPermissionDto();
        result.setScope(source.getScope());
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
