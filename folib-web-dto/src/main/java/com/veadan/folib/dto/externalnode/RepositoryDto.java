package com.veadan.folib.dto.externalnode;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;


/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class RepositoryDto implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 仓库名称
     */
    @ApiModelProperty("name")
    private String name;
    /**
     * 制品库类型
     */
    @ApiModelProperty("artifactoryRepositoryType")
    private String artifactoryRepositoryType;
    /**
     * key
     */
    @ApiModelProperty("key")
    private String key;
}
