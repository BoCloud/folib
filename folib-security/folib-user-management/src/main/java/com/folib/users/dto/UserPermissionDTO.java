package com.folib.users.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Collection;

/**
 * @author Veadan
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class UserPermissionDTO
{

    @NotEmpty(message = "Username is required!")
    private String userId;

    private Collection<String> privileges;

    @NotNull(message = "roleIds is required!")
    private Collection<String> roleIds;

    public @NotEmpty(message = "Username is required!") String getUserId() {
        return userId;
    }

    public void setUserId(@NotEmpty(message = "Username is required!") String userId) {
        this.userId = userId;
    }

    public Collection<String> getPrivileges() {
        return privileges;
    }

    public void setPrivileges(Collection<String> privileges) {
        this.privileges = privileges;
    }

    public @NotNull(message = "roleIds is required!") Collection<String> getRoleIds() {
        return roleIds;
    }

    public void setRoleIds(@NotNull(message = "roleIds is required!") Collection<String> roleIds) {
        this.roleIds = roleIds;
    }
}
