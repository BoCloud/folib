package com.folib.domain;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.folib.data.domain.DomainObject;

/**
 * @author veadan
 */
@JsonTypeInfo(
        use = JsonTypeInfo.Id.NAME,
        include = JsonTypeInfo.As.PROPERTY,
        property = "type"
)
@JsonSubTypes({
        @JsonSubTypes.Type(value = SecurityRoleEntity.class, name = "concrete")
})
public interface SecurityRole extends DomainObject
{

    default String getRoleName()
    {
        return getUuid();
    }
}
