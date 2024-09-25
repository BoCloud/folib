package com.veadan.folib.controllers.unicom;

import lombok.Data;

import java.util.List;

/**
 * @author huayanjun
 * @since 2024-09-24 16:15
 */

@Data
public class UnicomPermissionDTO {

    private String projectId;

    private List<String> users;

    private String admin;
}
