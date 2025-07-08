package com.folib.converters.configuration;

import com.folib.storage.StorageDto;
import com.folib.storage.repository.RepositoryDto;
import com.folib.forms.configuration.RepositoryForm;
import com.folib.forms.configuration.StorageForm;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.springframework.core.convert.converter.Converter;

/**
 * @author veadan
 */
public enum StorageFormConverter
        implements Converter<StorageForm, StorageDto>
{
    INSTANCE;

    @Override
    public StorageDto convert(final StorageForm source)
    {
        StorageDto result = new StorageDto();
        result.setBasedir(source.getBasedir());
        result.setId(source.getId());
        result.setAdmin(source.getAdmin());
        result.setStorageProvider(source.getStorageProvider());
        result.setUsers(source.getUsers());
        List<RepositoryForm> repositories = source.getRepositories();
        if (repositories != null)
        {
            Map<String, RepositoryDto> internalMap = new LinkedHashMap<>();
            result.setRepositories(internalMap);
            repositories.stream()
                        .map(RepositoryFormConverter.INSTANCE::convert)
                        .forEach(mr -> internalMap.put(mr.getId(), mr));
        }
        return result;
    }
}
