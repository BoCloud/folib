package com.folib.forms.users.auth;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.folib.validation.UniqueRoleName;
import lombok.Data;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Veadan
 */
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class RoleForm
{

    @NotBlank(groups = {NewRole.class, UpdateRole.class}, message = "A name must be specified.")
    @UniqueRoleName(groups = {NewRole.class}, message = "Role is already registered.")
    private String name;

    private String description;

    @Valid
    private AccessModelForm privileges;

    private List<AccessResources> resources;

    private List<String> access = new ArrayList<>();
    public interface NewRole {

    }
    public interface UpdateRole {

    }
}
