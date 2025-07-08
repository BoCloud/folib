package com.folib.controllers.adapter.jfrog.req;


import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.util.List;
import java.util.Map;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class PermissionTargetReq {

    @NotBlank
    private String name;
    private String includesPattern;
    private String excludesPattern;
    @NotEmpty
    private List<String> repositories;
    @Valid
    private Principals principals;

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Principals{

        private Map<String,List<String>> users;
        private Map<String,List<String>> groups;
    }

}
