package com.veadan.folib.users.domain;

import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.concurrent.Immutable;

import com.veadan.folib.domain.User;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.dto.UsersDto;

import com.google.common.collect.ImmutableSet;

/**
 * @author Przemyslaw Fusik
 */
@Immutable
public class Users
{

    private final Set<User> users;

    public Users(Set<User> source)
    {
        this.users = source != null ? ImmutableSet.copyOf(source) : Collections.emptySet();
    }

    public Users(final UsersDto source)
    {
        this.users = immuteUsers(source.getUsers());
    }

    private Set<User> immuteUsers(final Set<UserDto> source)
    {
        return source != null ? ImmutableSet.copyOf(source.stream()
                                                          .map(UserData::new)
                                                          .collect(
                                                                   Collectors.toSet()))
                : Collections.emptySet();
    }

    public Set<User> getUsers()
    {
        return users;
    }
}
