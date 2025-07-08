package com.folib.controllers.users.support;

import com.folib.authorization.domain.RoleData;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Set;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Steve Todorov
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class AssignableRoleResponseEntity
{

    @JsonProperty("assignableRoles")
    private ArrayList<AssignableRoleOutput> assignableRoles;

    public AssignableRoleResponseEntity(ArrayList<AssignableRoleOutput> assignableRoles)
    {
        this.assignableRoles = assignableRoles;
    }

    @JsonIgnore
    public AssignableRoleResponseEntity(Set<RoleData> assignableRoles)
    {
        this.assignableRoles = assignableRoles.stream()
                                              .sorted(Comparator.comparing(RoleData::getName))
                                              .map(AssignableRoleOutput::fromRole)
                                              .collect(Collectors.toCollection(ArrayList::new));
    }
}
