package com.veadan.folib.converters.configuration;

import com.veadan.folib.forms.configuration.GoRepositoryConfigurationForm;
import com.veadan.folib.yaml.configuration.repository.GoRepositoryConfigurationDto;
import org.springframework.core.convert.converter.Converter;

/**
 * @author pengYongQiang
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
