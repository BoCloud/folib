package com.veadan.folib.users.service.impl;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.io.IOException;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.HashSet;
import java.util.Map;
import java.util.function.Consumer;

import javax.inject.Inject;
import javax.inject.Qualifier;

import com.veadan.folib.users.UsersFileManager;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.dto.UsersDto;
import com.veadan.folib.users.service.impl.YamlUserService.Yaml;
import org.springframework.stereotype.Service;

/**
 * @author Alex Oreshkevich
 * @author veadan
 */
@Service
@Yaml
public class YamlUserService
        extends InMemoryUserService
{

    @Inject
    private UsersFileManager usersFileManager;

    @Override
    protected void modifyInLock(Consumer<Map<String, UserDto>> operation)
    {
        super.modifyInLock(operation.andThen(u -> doStoreUsers()));
    }

    private void doStoreUsers()
    {
        try
        {
            usersFileManager.store(new UsersDto(new HashSet<>(userMap.values())));
        }
        catch (IOException e)
        {
            throw new UndeclaredThrowableException(e);
        }
    }

    public void setUsers(final UsersDto newUsers)
    {
        modifyInLock(users -> {
            users.clear();
            newUsers.getUsers().stream().forEach(u -> users.put(u.getUsername(), u));
        });
    }

    @Documented
    @Retention(RUNTIME)
    @Qualifier
    public @interface Yaml
    {
    }
}
