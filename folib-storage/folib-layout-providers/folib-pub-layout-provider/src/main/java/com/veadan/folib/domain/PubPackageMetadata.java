package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * @author leipenghui
 * @date 2024/6/13
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PubPackageMetadata implements Serializable {

    /**
     * 项目名称。
     */
    private String name;

    /**
     * 最新版本信息。
     */
    private PubPackageVersionMetadata latest;

    /**
     * 版本列表。
     */
    private List<PubPackageVersionMetadata> versions;

    /**
     * 项目是否已停用。
     */
    private Boolean discontinued;
}
