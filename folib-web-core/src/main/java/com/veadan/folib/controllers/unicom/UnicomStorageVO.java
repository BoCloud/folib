package com.veadan.folib.controllers.unicom;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * @author huayanjun
 * @since 2024-09-23 15:45
 */
@Data
public class UnicomStorageVO {
    List<ProjectLayout> layouts;

    @Data
    public static class ProjectLayout{
        private String name;
        private String address;
    }
}
