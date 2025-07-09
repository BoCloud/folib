package com.folib.converters.configuration;

import com.folib.configuration.GoRepositoryConfigurationDto;
import com.folib.forms.configuration.GoRepositoryConfigurationForm;
import org.springframework.core.convert.converter.Converter;

/**
 * @author veadan
 * @date 1/16/2024 10:34
 */
public enum GoRepositoryConfigurationConverter  implements Converter<GoRepositoryConfigurationForm, GoRepositoryConfigurationDto> {

    INSTANCE;
    @Override
    public GoRepositoryConfigurationDto convert(GoRepositoryConfigurationForm source) {
        GoRepositoryConfigurationDto goRepositoryConfigurationDto = new GoRepositoryConfigurationDto();
        goRepositoryConfigurationDto.setGitVCS(source.getGitVCS());
        return goRepositoryConfigurationDto;
    }
}
