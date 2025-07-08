package com.folib.controllers.adapter.jfrog.res;

import lombok.Builder;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

@Data
@Builder
@Accessors(chain = true)
public class UserRes {

    private String name;
    private String email;
    private Boolean admin;
    private Boolean profileUpdatable;
    private Boolean disableUIAccess;
    private Boolean internalPasswordDisabled;
    private String lastLoggedIn;
    private String realm;
    private List<String> groups;
    private Boolean watchManager;
    private Boolean policyManager;

}
