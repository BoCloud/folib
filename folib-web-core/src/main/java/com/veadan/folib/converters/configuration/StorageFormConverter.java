package com.veadan.folib.converters.configuration;

import com.veadan.folib.forms.configuration.RepositoryForm;
import com.veadan.folib.forms.configuration.StorageForm;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.springframework.core.convert.converter.Converter;

/**
 * @author veadan
 */
public enum StorageFormConverter
        implements Converter<StorageForm, com.veadan.folib.storage.StorageDto>
{
    INSTANCE;

    @Override
    public com.veadan.folib.storage.StorageDto convert(final StorageForm source)
    {
        com.veadan.folib.storage.StorageDto result = new com.veadan.folib.storage.StorageDto();
        result.setBasedir(source.getBasedir());
        result.setId(source.getId());
        result.setAdmin(source.getAdmin());
        result.setStorageProvider(source.getStorageProvider());
        result.setUsers(source.getUsers());
        List<RepositoryForm> repositories = source.getRepositories();
        if (repositories != null)
        {
            Map<String, com.veadan.folib.storage.repository.RepositoryDto> internalMap = new LinkedHashMap<>();
            result.setRepositories(internalMap);
            repositories.stream()
                        .map(RepositoryFormConverter.INSTANCE::convert)
                        .forEach(mr -> internalMap.put(mr.getId(), mr));
        }
        return result;
    }
}
