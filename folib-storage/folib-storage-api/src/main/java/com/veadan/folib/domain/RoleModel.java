package com.veadan.folib.domain;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RoleModel {

    private String name;

    private String description;

    private AccessModel accessModel;


}
