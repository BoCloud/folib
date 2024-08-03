package com.veadan.folib.forms.users.auth;

import lombok.Data;

import java.util.List;

/**
 * @author veadan
 */
@Data
public class AccessModelForm
{
    private List<AccessUserGroups> groups;
    private List<AccessUsers> users;

    //private List<AccessResources> resources;

}
