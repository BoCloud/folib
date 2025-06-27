package com.veadan.folib.domain.adapter.jfrog;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DockerBlobsInfo {

    /**
     * 大小
     */
    private String size;

    /**
     * 创建时间
     */
    private String created;

    /**
     * 摘要
     */
    private String digest;

    /**
     * 命令
     */
    private String command;

    /**
     * 命令文本
     */
    private String commandText;
}

