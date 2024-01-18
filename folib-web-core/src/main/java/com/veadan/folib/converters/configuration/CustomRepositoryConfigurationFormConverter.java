package com.veadan.folib.converters.configuration;

import com.veadan.folib.forms.configuration.*;
import com.veadan.folib.yaml.configuration.repository.GoRepositoryConfigurationDto;
import com.veadan.folib.yaml.configuration.repository.MavenRepositoryConfigurationDto;
import com.veadan.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto;
import com.veadan.folib.yaml.configuration.repository.RawRepositoryConfigurationDto;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

/**
 * @author veadan
 */
public enum CustomRepositoryConfigurationFormConverter
        implements CustomRepositoryConfigurationFormVisitor<CustomRepositoryConfigurationDto>
{
    INSTANCE;

    public MavenRepositoryConfigurationDto visit(MavenRepositoryConfigurationForm form)
    {
        return MavenRepositoryConfigurationConverter.INSTANCE.convert(form);
    }

    public NugetRepositoryConfigurationDto visit(NugetRepositoryConfigurationForm form)
    {
        return NugetRepositoryConfigurationConverter.INSTANCE.convert(form);
    }

    public RawRepositoryConfigurationDto visit(RawRepositoryConfigurationForm form)
    {
        return RawRepositoryConfigurationConverter.INSTANCE.convert(form);
    }

    public GoRepositoryConfigurationDto visit(GoRepositoryConfigurationForm form)
    {
        return GoRepositoryConfigurationConverter.INSTANCE.convert(form);
    }

}
