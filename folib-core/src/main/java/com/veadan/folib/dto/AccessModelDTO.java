package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AccessModelDTO
{
    private List<ApiAccess> apiAccess = new ArrayList<>();
    
    private List<RepositoryAccessModelDTO> repositoriesAccess = new ArrayList<>();

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ApiAccess {
        String api;
        Long resourceId;
    }
}
