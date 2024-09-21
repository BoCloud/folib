package com.veadan.folib.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Veadan
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class RoleDTO
{

    private String name;

    private String description;

    private AccessModelDTO privileges;

    private List<AccessResourcesDTO> resources;

    private List<String> access = new ArrayList<>();
    public interface NewRole {

    }
}
