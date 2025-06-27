package com.veadan.folib.dto.packagenameblock;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author veadan
 * @date 2023/10/24
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("PackageNameBlock")
public class PackageNameBlockDto implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    private Long id;

    /**
     * 包名
     */
    @ApiModelProperty("包名")
    private String packageName;

    /**
     * 条件 range（范围）eq（等于）
     */
    @ApiModelProperty("条件")
    private String conditionValue;

    /**
     * 版本
     */
    @ApiModelProperty("版本")
    private String version;

    /**
     * 包名阻断列表
     */
    @ApiModelProperty("包名阻断列表")
    private List<PackageNameBlockDto> packageNameBlocks;

    /**
     * 包名
     */
    @ApiModelProperty("包名")
    private List<String> packageNames;
}
