package com.veadan.folib.dto.customlayout;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;

/**
 * @author leipenghui
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("CustomLayoutForm")
public class CustomLayoutDto implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private String id;

    /**
     * 布局名称
     */
    @ApiModelProperty("布局名称")
    @NotBlank(message = "请填写布局名称", groups = {SaveGroup.class, UpdateGroup.class, DeleteGroup.class})
    private String layoutName;

    /**
     * 模糊布局名称
     */
    private String matchLayoutName;

    /**
     * 制品路径正则表达式
     */
    @ApiModelProperty("制品路径正则表达式")
    @NotBlank(message = "请填写制品路径正则表达式", groups = {SaveGroup.class, UpdateGroup.class})
    private String artifactPathPattern;

    public interface SaveGroup
            extends Serializable {
        // 新增组
    }

    public interface UpdateGroup
            extends Serializable {
        // 更新组
    }

    public interface DeleteGroup
            extends Serializable {
        // 删除组
    }
}
