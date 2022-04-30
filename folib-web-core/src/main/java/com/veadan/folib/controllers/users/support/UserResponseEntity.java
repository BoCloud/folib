package com.veadan.folib.controllers.users.support;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import com.veadan.folib.authorization.domain.RoleData;

import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * @author Steve Todorov
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserResponseEntity
{
    private UserOutput user;

    private List<AssignableRoleOutput> assignableRoles;

    public UserResponseEntity()
    {
    }

    public UserResponseEntity(UserOutput user)
    {
        this.setUser(user);
    }

    public UserOutput getUser()
    {
        return user;
    }

    public void setUser(UserOutput user)
    {
        this.user = user;
    }

    public List<AssignableRoleOutput> getAssignableRoles()
    {
        return assignableRoles;
    }

    public void setAssignableRoles(Set<RoleData> assignableRoles)
    {
        this.assignableRoles = assignableRoles.stream()
                                              .sorted(Comparator.comparing(RoleData::getName))
                                              .map(AssignableRoleOutput::fromRole)
                                              .collect(Collectors.toCollection(ArrayList::new));
    }

}
