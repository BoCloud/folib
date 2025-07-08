package com.folib.forms.artifact;

import com.folib.enums.ArtifactMetadataEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @author leipenghui
 * @date 2022/11/29
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactMetadataForm {

    /**
     * 元数据类型
     *
     * @see ArtifactMetadataEnum
     */
    @NotBlank(message = "请填写元数据类型", groups = {ConfigurationAddOrUpdateGroup.class, AddOrUpdateGroup.class})
    private String type;
    /**
     * 前端是否展示
     */
    @NotNull(message = "请选择前端是否展示", groups = {ConfigurationAddOrUpdateGroup.class, AddOrUpdateGroup.class})
    @Min(value = 0, message = "前端是否展示仅为0或1", groups = {ConfigurationAddOrUpdateGroup.class, AddOrUpdateGroup.class})
    @Max(value = 1, message = "前端是否展示仅为0或1", groups = {ConfigurationAddOrUpdateGroup.class, AddOrUpdateGroup.class})
    private Integer viewShow;
    /**
     * 元数据key
     */
    @NotBlank(message = "请填写元数据key", groups = {ConfigurationAddOrUpdateGroup.class, ConfigurationDeleteGroup.class, AddOrUpdateGroup.class, DeleteGroup.class})
    private String key;
    /**
     * 元数据值
     */
    @NotBlank(message = "请填写元数据值", groups = {AddOrUpdateGroup.class})
    private String value;
    /**
     * 展示位置
     */
    private String location;
    /**
     * 存储空间名称
     */
    @NotBlank(message = "存储空间名称不能为空", groups = {AddOrUpdateGroup.class, DeleteGroup.class})
    private String storageId;
    /***
     * 仓库名称
     */
    @NotBlank(message = "仓库名称不能为空", groups = {AddOrUpdateGroup.class, DeleteGroup.class})
    private String repositoryId;
    /**
     * 制品路径
     */
    @NotBlank(message = "制品路径不能为空", groups = {AddOrUpdateGroup.class, DeleteGroup.class})
    private String artifactPath;

    private Boolean recursive;

    public interface ConfigurationAddOrUpdateGroup
            extends Serializable {
        // 配置新增组
    }

    public interface ConfigurationDeleteGroup
            extends Serializable {
        // 配置删除组
    }

    public interface AddOrUpdateGroup
            extends Serializable {
        // 新增组
    }

    public interface DeleteGroup
            extends Serializable {
        // 删除组
    }
}
