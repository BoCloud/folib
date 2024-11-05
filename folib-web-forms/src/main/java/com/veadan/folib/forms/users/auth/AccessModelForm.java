package com.veadan.folib.forms.users.auth;

import lombok.Data;

import javax.validation.Valid;
import java.util.List;

/**
 * @author veadan
 */
@Data
public class AccessModelForm
{
    @Valid
    private List<AccessUserGroups> groups;

    @Valid
    private List<AccessUsers> users;

    //private List<AccessResources> resources;

}
