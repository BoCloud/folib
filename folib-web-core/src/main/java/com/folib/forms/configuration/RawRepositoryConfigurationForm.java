package com.folib.forms.configuration;

import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * @author veadan
 */
@JsonTypeName("Raw")
public class RawRepositoryConfigurationForm
        extends CustomRepositoryConfigurationForm
{

    @Override
    public <T> T accept(final CustomRepositoryConfigurationFormVisitor<T> visitor)
    {
        return visitor.visit(this);
    }

}
