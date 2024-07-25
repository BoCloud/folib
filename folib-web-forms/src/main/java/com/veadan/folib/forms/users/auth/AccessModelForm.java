package com.veadan.folib.forms.users.auth;

import lombok.Data;

import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;

/**
 * @author veadan
 */
@Data
public class AccessModelForm
{


    private List<ApiAccess> apiAccess = new ArrayList<>();
    
    @Valid
    private List<RepositoryAccessModelForm> repositoriesAccess = new ArrayList<>();

    @Data
    public static class ApiAccess {
        String api;
        Long resourceId;
    }
}
