package com.veadan.folib.domain;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.veadan.folib.data.domain.DomainObject;

/**
 * @author ankit.tomar
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
