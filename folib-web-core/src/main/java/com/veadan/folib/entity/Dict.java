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
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "dict")
@ApiModel("dict")
public class Dict implements Serializable {
    private static final long serialVersionUID = 1L;


    @Id
    @GeneratedValue(generator = "JDBC",strategy = GenerationType.IDENTITY)
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
     * 字典类型
     */
    @ApiModelProperty("字典类型")
    @Column(name = "dict_type")
    private String dictType;

    /**
     * 字典key
     */
    @ApiModelProperty("字典key")
    @Column(name = "dict_key")
    private String dictKey;

    /**
     * 字典值
     */
    @ApiModelProperty("字典值")
    @Column(name = "dict_value")
    private String dictValue;

    /**
     * 别名
     */
    @ApiModelProperty("别名")
    @Column(name = "alias")
    private String alias;

    /**
     * 备注
     */
    @ApiModelProperty("备注")
    @Column(name = "\"comment\"")
    private String comment;
}
