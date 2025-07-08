package com.folib.domain.adapter.jfrog;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2023/10/8
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DockerImageInfo {

    /**
     * 标签信息
     */
    private DockerTagInfo tagInfo;

    /**
     * blob 信息列表
     */
    private List<DockerBlobsInfo> blobsInfo;
}
