package com.veadan.folib.users.security;

import java.util.function.Function;

import javax.annotation.concurrent.Immutable;

import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.users.dto.AccessModel;

/**
 * @author xuxinping
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
