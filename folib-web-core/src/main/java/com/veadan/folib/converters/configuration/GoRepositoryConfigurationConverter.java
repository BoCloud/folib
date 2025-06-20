package com.veadan.folib.converters.configuration;

import com.veadan.folib.dto.configuration.GoRepositoryConfigurationDto;
import org.springframework.core.convert.converter.Converter;

/**
 * @author pengYongQiang
 * @date 1/16/2024 10:34
 */
public enum GoRepositoryConfigurationConverter  implements Converter<GoRepositoryConfigurationDto, com.veadan.folib.yaml.configuration.repository.GoRepositoryConfigurationDto> {

    INSTANCE;
    @Override
    public com.veadan.folib.yaml.configuration.repository.GoRepositoryConfigurationDto convert(GoRepositoryConfigurationDto source) {
        com.veadan.folib.yaml.configuration.repository.GoRepositoryConfigurationDto goRepositoryConfigurationDto = new com.veadan.folib.yaml.configuration.repository.GoRepositoryConfigurationDto();
        goRepositoryConfigurationDto.setGitVCS(source.getGitVCS());
        return goRepositoryConfigurationDto;
    }
}
