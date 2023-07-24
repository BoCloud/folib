package com.veadan.folib.authorization.service.impl;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;

import javax.inject.Inject;

import com.veadan.folib.authorization.domain.Client;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import com.veadan.folib.authorization.AuthorizationConfigFileManager;
import com.veadan.folib.authorization.domain.AuthorizationConfig;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import org.springframework.stereotype.Service;


/**
 * @author 
 * @author veadan
 */
@Service
public class AuthorizationConfigServiceImpl
        implements AuthorizationConfigService
{

    private final ReadWriteLock authorizationConfigLock = new ReentrantReadWriteLock();

    @Inject
    private AuthorizationConfigFileManager authorizationConfigFileManager;

    /**
     * Yes, this is a state object.
     * It is protected by the {@link #authorizationConfigLock} here
     * and should not be exposed to the world.
     */
    private AuthorizationConfigDto authorizationConfig;

    @Override
    public void setAuthorizationConfig(final AuthorizationConfigDto newConfig) throws IOException
    {
        modifyInLock(config ->
                     {
                         AuthorizationConfigServiceImpl.this.authorizationConfig = newConfig;
                     },
                     true);
    }

    @Override
    public AuthorizationConfigDto getDto()
    {
        final Lock readLock = authorizationConfigLock.readLock();
        readLock.lock();

        try
        {
            return SerializationUtils.clone(authorizationConfig);
        }
        finally
        {
            readLock.unlock();
        }
    }

    @Override
    public AuthorizationConfig get()
    {
        final Lock readLock = authorizationConfigLock.readLock();
        readLock.lock();

        try
        {
            return new AuthorizationConfig(authorizationConfig);
        }
        finally
        {
            readLock.unlock();
        }
    }

    @Override
    public void addRole(final RoleDto role) throws IOException
    {
        modifyInLock(config ->
                     {
                         config.getRoles().add(role);
                     });
    }

    public void addClient(final Client client) throws IOException
    {
        modifyInLock(config ->
        {
            config.getClients().add(client);
        });
    }

    public boolean deleteClient(final String clientId) throws IOException
    {
        MutableBoolean result = new MutableBoolean();
        modifyInLock(config ->
        {
            Set<Client> clients = config.getClients();
            clients.stream()
                    .filter(r -> r.getClientId()
                            .equalsIgnoreCase(clientId))
                    .findFirst()
                    .ifPresent(r -> {
                        result.setValue(clients.remove(r));}
                            );
        });
        return result.isTrue();
    }



    @Override
    public boolean deleteRole(final String roleName) throws IOException
    {
        MutableBoolean result = new MutableBoolean();
        modifyInLock(config ->
                     {
                         Set<RoleDto> roles = config.getRoles();
                         roles.stream()
                              .filter(r -> r.getName()
                                            .equalsIgnoreCase(roleName))
                              .findFirst()
                              .ifPresent(r -> result.setValue(roles.remove(r)));
                     });
        return result.isTrue();
    }

    @Override
    public void addPrivilegesToAnonymous(final List<Privileges> privilegeList) throws IOException
    {
        modifyInLock(config ->
                     {
                         Set<RoleDto> roles = config.getRoles();
                         roles.stream()
                              .filter(r -> r.getName()
                                            .equalsIgnoreCase(SystemRole.ANONYMOUS.name()))
                              .findFirst()
                              .ifPresent(r -> privilegeList.stream()
                                                           .forEach(p -> r.addPrivilege(p)));
                     });
    }

    private void modifyInLock(final Consumer<AuthorizationConfigDto> operation) throws IOException
    {
        modifyInLock(operation, true);
    }

    private void modifyInLock(final Consumer<AuthorizationConfigDto> operation,
                              final boolean storeInFile) throws IOException
    {
        final Lock writeLock = authorizationConfigLock.writeLock();
        writeLock.lock();

        try
        {
            operation.accept(authorizationConfig);

            if (storeInFile)
            {
                authorizationConfigFileManager.store(authorizationConfig);
            }
        }
        finally
        {
            writeLock.unlock();
        }
    }

}
