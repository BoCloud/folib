package com.veadan.folib.domain.adapter.jfrog;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DockerTagInfo {

    /**
     * 标签标题
     */
    private String title;

    /**
     * 标签的摘要
     */
    private String digest;

    /**
     * 总大小
     */
    private String totalSize;

    /**
     * 长整型的总大小
     */
    private long totalSizeLong;

    /**
     * 端口列表
     */
    private List<String> ports;

    /**
     * 卷列表
     */
    private List<String> volumes;

    /**
     * 标签列表
     */
    private List<String> labels;
}
