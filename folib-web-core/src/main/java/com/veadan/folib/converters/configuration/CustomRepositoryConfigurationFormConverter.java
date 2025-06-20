package com.veadan.folib.converters.configuration;

import com.veadan.folib.dto.configuration.*;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

/**
 * @author veadan
 */
public enum CustomRepositoryConfigurationFormConverter
        implements CustomRepositoryConfigurationDtoVisitor<CustomRepositoryConfigurationDto>
{
    INSTANCE;

    public com.veadan.folib.yaml.configuration.repository.MavenRepositoryConfigurationDto visit(MavenRepositoryConfigurationDto form)
    {
        return MavenRepositoryConfigurationConverter.INSTANCE.convert(form);
    }

    public com.veadan.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto visit(NugetRepositoryConfigurationDto form)
    {
        return NugetRepositoryConfigurationConverter.INSTANCE.convert(form);
    }

    public com.veadan.folib.yaml.configuration.repository.RawRepositoryConfigurationDto visit(RawRepositoryConfigurationDto form)
    {
        return RawRepositoryConfigurationConverter.INSTANCE.convert(form);
    }

    public com.veadan.folib.yaml.configuration.repository.GoRepositoryConfigurationDto visit(GoRepositoryConfigurationDto form)
    {
        return GoRepositoryConfigurationConverter.INSTANCE.convert(form);
    }

}
