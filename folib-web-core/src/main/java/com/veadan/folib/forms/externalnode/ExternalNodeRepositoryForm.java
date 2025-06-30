package com.veadan.folib.forms.externalnode;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;


/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class ExternalNodeRepositoryForm implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @ApiModelProperty("id")
    private Long id;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /**
     * 节点名称
     */
    @ApiModelProperty("节点名称")
    private String nodeName;

    /**
     * key
     */
    @ApiModelProperty("key")
    private String key;

    /**
     * 制品库类型
     */
    @ApiModelProperty("制品库类型")
    private String type;

    /**
     * 节点地址
     */
    @ApiModelProperty("节点地址")
    private String address;

    /**
     * 备注
     */
    @ApiModelProperty("备注")
    private String comment;

    /**
     * 仓库列表
     */
    @ApiModelProperty("仓库列表")
    private List<RepositoryForm> repositories;
}
