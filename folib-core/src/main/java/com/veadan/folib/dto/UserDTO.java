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

    private Set<String> roles;

    private String securityTokenKey;
    private String userGroups;

    private LinkedHashSet<String> authorities;

    public void setRoles(String roles) {
        if (roles != null) {
            this.roles = new HashSet<>(Arrays.asList(roles.split(",")));
        }
    }
}
