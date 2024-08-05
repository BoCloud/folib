package com.veadan.folib.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * @author veadan
 * @author Veadan
 * @JsonInclude used because com.veadan.folib.users.domain.User is annotated with it
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserDTO
        implements Serializable
{

    private String username;
    private String password;

    private boolean enabled;

    private String email;

    private String avatar;

    private Set<String> userGroups;
    private Set<String> userGroupIds;
    private Set<String> roles;

    private String securityTokenKey;

    private LinkedHashSet<String> authorities;
    public void setUserGroups(String userGroups) {
        if (userGroups != null) {
            this.userGroups = new HashSet<>(Arrays.asList(userGroups.split(",")));
        }
    }
    public void setRoles(String roles) {
        if (roles != null) {
            this.roles = new HashSet<>(Arrays.asList(roles.split(",")));
        }
    }

    public void setUserGroupIds(String userGroupIds) {
        if (userGroupIds != null) {
            this.userGroupIds = new HashSet<>(Arrays.asList(userGroupIds.split(",")));
        }
    }
}
