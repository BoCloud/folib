package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2023/6/20
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DockerTags {

    /**
     * 镜像名称
     */
    private String name;

    /**
     * tag列表
     */
    private List<String> tags;
}
