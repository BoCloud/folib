package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author qijianping
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class TargetRepositoyDto {

    /**
     * 目标存储id
     */
    private String targetStorageId;

    /**
     * 目标仓库id
     */
    private String targetRepositoryId;
}
