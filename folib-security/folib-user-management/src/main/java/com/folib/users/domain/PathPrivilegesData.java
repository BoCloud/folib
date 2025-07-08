package com.folib.users.domain;

import java.io.Serializable;
import java.util.Collections;
import java.util.Set;

import javax.annotation.concurrent.Immutable;

import com.folib.users.dto.PathPrivileges;
import com.folib.users.dto.PathPrivilegesDto;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
@Immutable
public class PathPrivilegesData
        implements Serializable, PathPrivileges
{

    private final String path;

    /**
     *  If true, allows to set privileges within path and all subdirectories
     */
    private final boolean wildcard;

    private final Set<Privileges> privileges;

    public PathPrivilegesData(final PathPrivilegesDto delegate)
    {
        this.path = delegate.getPath();
        this.wildcard = delegate.isWildcard();
        this.privileges = immutePrivileges(delegate.getPrivileges());
    }


    private Set<Privileges> immutePrivileges(final Set<Privileges> source)
    {
        return source != null ? ImmutableSet.copyOf(source) : Collections.emptySet();
    }

    public String getPath()
    {
        return path;
    }

    public boolean isWildcard()
    {
        return wildcard;
    }

    public Set<Privileges> getPrivileges()
    {
        return privileges;
    }
}
