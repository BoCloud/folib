package com.veadan.folib.authorization.domain;

import javax.annotation.concurrent.Immutable;

import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.users.domain.AccessModelData;
import com.veadan.folib.users.dto.AccessModel;
import com.veadan.folib.users.dto.AccessModelDto;

/**
 * @author Przemyslaw Fusik
 */
@Immutable
public class RoleData implements Role
{

    private final String name;

    private final String description;

    private final AccessModel accessModel;

    public RoleData(final RoleDto source)
    {
        this.name = source.getName();
        this.description = source.getDescription();
        this.accessModel = immuteAccessModel(source.getAccessModel());
    }

    private AccessModel immuteAccessModel(AccessModelDto source)
    {
        return source != null ? new AccessModelData(source) : null;
    }

    public String getName()
    {
        return name;
    }

    public String getDescription()
    {
        return description;
    }

    public AccessModel getAccessModel()
    {
        return accessModel;
    }

}
