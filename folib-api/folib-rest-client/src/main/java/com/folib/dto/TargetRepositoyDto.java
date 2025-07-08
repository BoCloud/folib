package com.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2022-11-18
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