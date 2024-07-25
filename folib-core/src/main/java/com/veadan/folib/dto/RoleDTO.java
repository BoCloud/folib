package com.veadan.folib.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

import java.util.List;

/**
 * @author Veadan
 */
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class RoleDTO
{

    private List<String> userIds;

    private List<Long> userGroupIds;

    private String name;

    private String description;

    private AccessModelDTO accessModel;

    
}
