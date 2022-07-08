package com.veadan.folib.authorization.domain;

import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.RoleDto;

import javax.annotation.concurrent.Immutable;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
@Immutable
public class AuthorizationConfig
{

    private final Set<RoleData> roles;

    public AuthorizationConfig(final AuthorizationConfigDto source)
    {
        this.roles = immuteRoles(source.getRoles());
    }

    private Set<RoleData> immuteRoles(final Set<RoleDto> source)
    {
        return source != null ? ImmutableSet.copyOf(source.stream().map(RoleData::new).collect(
                Collectors.toSet())) : Collections.emptySet();
    }

    public Set<RoleData> getRoles()
    {
        return roles;
    }
}

