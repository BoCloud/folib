package com.folib.forms.users.auth;

import lombok.Data;

import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;

/**
 * @author veadan
 */
@Data
public class Access
{

    private String userId;

    private Long userGroupId;

    private List<ApiAccess> apiAccess = new ArrayList<>();
    private List<String> privileges;

    @Valid
    private List<RepositoryAccessModelForm> repositoriesAccess = new ArrayList<>();

    @Data
    public static class ApiAccess {
        String api;
        Long resourceId;
    }
}
