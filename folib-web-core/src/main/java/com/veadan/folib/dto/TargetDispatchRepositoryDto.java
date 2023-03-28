package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class TargetDispatchRepositoryDto {

    /**
     * 集群英文名
     */
    private String dispatchClusterEnName;

    /**
     * 目标存储id
     */
    private String targetStorageId;

    /**
     * 目标仓库id
     */
    private String targetRepositoryId;
}
