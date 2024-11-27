package com.veadan.folib.dto;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/**
 * @author veadan
 */
@Data
public class AccessDTO
{

    private String userId;

    private Long userGroupId;

    private List<ApiAccess> apiAccess = new ArrayList<>();
    private List<String> privileges;

    private List<RepositoryAccessModelDTO> repositoriesAccess = new ArrayList<>();

    @Data
    public static class ApiAccess {
        String api;
        Long resourceId;
    }
}
