package com.veadan.folib.domain;

import com.veadan.folib.data.domain.DomainObject;

/**
 * @author ankit.tomar
 */
public interface SecurityRole extends DomainObject
{

    default String getRoleName()
    {
        return getUuid();
    }
}
