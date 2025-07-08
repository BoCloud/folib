package com.folib.users.security;

import java.util.function.Function;

import javax.annotation.concurrent.Immutable;

import com.folib.authorization.dto.Role;
import com.folib.users.dto.AccessModel;

/**
 * @author veadan
 */
@Immutable
public class RuntimeRole implements Role
{

    private final Role target;
    private final Function<AccessModel, AccessModel> accessModelCustomizer;

    public RuntimeRole(Role target,
                       Function<AccessModel, AccessModel> accessModelCustomizer)
    {
        this.target = target;
        this.accessModelCustomizer = accessModelCustomizer;
    }

    public String getName()
    {
        return target.getName();
    }

    public String getDescription()
    {
        return target.getDescription();
    }

    public AccessModel getAccessModel()
    {
        return accessModelCustomizer.apply(target.getAccessModel());
    }

}
