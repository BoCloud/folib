package com.veadan.folib.dto.externalnode;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.veadan.folib.enums.ArtifactoryRepositoryTypeEnum;
import com.veadan.folib.dto.validate.SaveGroup;
import com.veadan.folib.dto.validate.DeleteGroup;
import com.veadan.folib.dto.validate.UpdateGroup;
import com.veadan.folib.validation.externalnode.ArtifactoryRepositoryEnumValue;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
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
public class ExternalNodeDto implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @ApiModelProperty("id")
    @NotNull(message = "请填写id", groups = {UpdateGroup.class, DeleteGroup.class})
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
    @NotBlank(message = "请填写节点名称", groups = {SaveGroup.class})
    @Length(max = 50, message = "节点名称不能超过50个字符")
    private String nodeName;

    /**
     * 制品库类型
     */
    @ApiModelProperty("制品库类型")
    @ArtifactoryRepositoryEnumValue(message = "制品库类型校验失败，请检查制品库类型.", type = ArtifactoryRepositoryTypeEnum.class)
    private String type;

    /**
     * 节点地址
     */
    @ApiModelProperty("节点地址")
    @NotBlank(message = "请填写节点地址", groups = {SaveGroup.class})
    @Length(max = 255, message = "节点地址不能超过255个字符")
    private String address;

    /**
     * 用户名
     */
    @ApiModelProperty("用户名")
    @NotBlank(message = "请填写用户名", groups = {SaveGroup.class})
    @Length(max = 50, message = "用户名不能超过50个字符")
    private String username;

    /**
     * 密码
     */
    @ApiModelProperty("密码")
    @NotBlank(message = "请填写密码", groups = {SaveGroup.class})
    private String password;

    /**
     * 备注
     */
    @ApiModelProperty("备注")
    private String comment;
}
