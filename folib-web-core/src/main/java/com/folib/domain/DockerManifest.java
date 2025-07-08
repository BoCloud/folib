package com.folib.domain;

import com.alibaba.fastjson.annotation.JSONField;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2023/2/15
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DockerManifest {

    @JSONField(name = "Config")
    private String config;

    @JSONField(name = "RepoTags")
    private List<String> repoTags;

    @JSONField(name = "Layers")
    private List<String> layers;
}
