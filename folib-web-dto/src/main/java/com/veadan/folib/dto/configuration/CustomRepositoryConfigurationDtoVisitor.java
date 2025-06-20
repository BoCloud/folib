package com.veadan.folib.dto.configuration;

/**
 * @author veadan
 */
public interface CustomRepositoryConfigurationDtoVisitor<T>
{

    T visit(MavenRepositoryConfigurationDto form);

    T visit(NugetRepositoryConfigurationDto form);

    T visit(RawRepositoryConfigurationDto form);

    T visit(GoRepositoryConfigurationDto form);
}
