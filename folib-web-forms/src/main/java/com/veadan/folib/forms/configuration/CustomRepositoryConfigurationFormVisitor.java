package com.veadan.folib.forms.configuration;

/**
 * @author veadan
 */
public interface CustomRepositoryConfigurationFormVisitor<T>
{

    T visit(MavenRepositoryConfigurationForm form);

    T visit(NugetRepositoryConfigurationForm form);

    T visit(RawRepositoryConfigurationForm form);

    T visit(GoRepositoryConfigurationForm form);
}
