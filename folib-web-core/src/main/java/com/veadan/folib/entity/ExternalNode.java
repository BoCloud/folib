package com.veadan.folib.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;


/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "external_node")
@ApiModel("ExternalNode")
public class ExternalNode implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @Id
    @GeneratedValue(generator = "JDBC", strategy = GenerationType.IDENTITY)
    @ApiModelProperty("id")
    @Column(name = "id")
    private Long id;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @Column(name = "create_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /**
     * 节点名称
     */
    @ApiModelProperty("节点名称")
    @Column(name = "node_name")
    private String nodeName;

    /**
     * 制品库类型
     */
    @ApiModelProperty("制品库类型")
    @Column(name = "type")
    private String type;

    /**
     * 节点地址
     */
    @ApiModelProperty("节点地址")
    @Column(name = "address")
    private String address;

    /**
     * 用户名
     */
    @ApiModelProperty("用户名")
    @Column(name = "username")
    private String username;

    /**
     * 密码
     */
    @ApiModelProperty("密码")
    @Column(name = "password")
    private String password;

    /**
     * 备注
     */
    @ApiModelProperty("备注")
    @Column(name = "comment")
    private String comment;
}
