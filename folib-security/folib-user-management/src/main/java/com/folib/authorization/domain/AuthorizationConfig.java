package com.folib.authorization.domain;

import com.folib.authorization.dto.AuthorizationConfigDto;
import com.folib.authorization.dto.RoleDto;

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

    private final Set<Client> clients;

    public AuthorizationConfig(final AuthorizationConfigDto source)
    {
        this.roles = immuteRoles(source.getRoles());
        this.clients= immuteClients(source.getClients());

    }



    private Set<RoleData> immuteRoles(final Set<RoleDto> source)
    {
        return source != null ? ImmutableSet.copyOf(source.stream().map(RoleData::new).collect(
                Collectors.toList())) : Collections.emptySet();
    }

    private Set<Client> immuteClients(final Set<Client> source)
    {
        return source != null ? ImmutableSet.copyOf(source.stream().map(Client::new).collect(
                Collectors.toList())) : Collections.emptySet();
    }



    public Set<RoleData> getRoles()
    {
        return roles;
    }

    public Set<Client> getClients()
    {
        return clients;
    }
}

