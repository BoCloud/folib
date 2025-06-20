package com.veadan.folib.dto.users.auth;

import lombok.Data;

import javax.validation.Valid;
import java.util.List;

/**
 * @author veadan
 */
@Data
public class AccessModelDto
{
    @Valid
    private List<AccessUserGroups> groups;

    @Valid
    private List<AccessUsers> users;

    //private List<AccessResources> resources;

}
