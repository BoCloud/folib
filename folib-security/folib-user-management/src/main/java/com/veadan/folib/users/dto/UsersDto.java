package com.veadan.folib.users.dto;

import java.util.Optional;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonRootName;
import org.apache.commons.collections4.CollectionUtils;

/**
 * @author mtodorov
 * @author Pablo Tirado
 */
@JsonRootName("users")
public class UsersDto
{

    @JsonProperty("user")
    private Set<UserDto> users;

    @JsonCreator
    public UsersDto(@JsonProperty("user") Set<UserDto> users)
    {
        this.users = users;
    }

    public Set<UserDto> getUsers()
    {
        return users;
    }

    public void setUsers(Set<UserDto> users)
    {
        this.users = users;
    }

    public Optional<UserDto> findByUserName(final String username)
    {
        if (CollectionUtils.isEmpty(users))
        {
            return Optional.empty();
        }
        if (username == null)
        {
            return Optional.empty();
        }
        return users.stream().filter(user -> username.equals(user.getUsername())).findFirst();
    }
}
