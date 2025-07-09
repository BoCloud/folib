package com.folib.converters.configuration;

import com.folib.forms.configuration.*;
import com.folib.configuration.GoRepositoryConfigurationDto;
import com.folib.configuration.MavenRepositoryConfigurationDto;
import com.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto;
import com.folib.configuration.RawRepositoryConfigurationDto;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;

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
