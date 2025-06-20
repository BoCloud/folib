package com.veadan.folib.dto.configuration;

import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * @author veadan
 */
@JsonTypeName("Raw")
public class RawRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto
{

    @Override
    public <T> T accept(final CustomRepositoryConfigurationDtoVisitor<T> visitor)
    {
        return visitor.visit(this);
    }

}
