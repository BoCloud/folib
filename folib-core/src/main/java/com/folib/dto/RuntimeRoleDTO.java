package com.folib.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Set;

/**
 * @author Veadan
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class RuntimeRoleDTO
{

    private RuntimeDTO target;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    private class RuntimeDTO {
        private String name;

        private String description;

        private AuthoritieDTO accessModel;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    private class AuthoritieDTO {
        Set<String> apiAuthorities;

        Set<String> pathAuthorities;
    }
}
