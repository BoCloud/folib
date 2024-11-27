package com.veadan.folib.controllers.adapter.jfrog.req;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

@Data
@Accessors(chain = true)
public class GroupReq {

    private String groupName;
    private String description;
    private List<String> usersInGroup;
    private boolean autoJoin;
    private boolean adminPrivileges;
    private boolean watchManager;
    private boolean reportsManager;
    private boolean policyManager;
    private boolean manageResources;
}
