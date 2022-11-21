package com.veadan.folib.converters.configuration;

import com.veadan.folib.forms.configuration.RepositoryForm;
import com.veadan.folib.forms.configuration.StorageForm;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.RepositoryDto;

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
