package com.veadan.folib.forms.users.auth;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.veadan.folib.validation.UniqueRoleName;
import lombok.Data;

import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * @author Veadan
 */
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class RoleForm
{
    private List<String> userIds;

    private List<Long> userGroupIds;

    @NotEmpty(message = "A name must be specified.")
    @UniqueRoleName(message = "Role is already registered.")
    private String name;

    private String description;

    private AccessModelForm accessModel;

    public interface NewRole {

    }
}
