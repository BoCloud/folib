package com.veadan.folib.forms.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UnionTargetRepositoryConfigurationForm
        implements Serializable {

    /**
     * 节点
     */
    @NotBlank(message = "节点不能为空")
    private String node;

    /**
     * 类型
     */
    private String type;

    /**
     * 存储空间
     */
    private String storageId;

    /**
     * 仓库名称
     */
    @NotBlank(message = "仓库名称不能为空")
    private String repositoryId;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof UnionTargetRepositoryConfigurationForm)) {
            return false;
        }
        UnionTargetRepositoryConfigurationForm that = (UnionTargetRepositoryConfigurationForm) o;
        return node.equals(that.node) &&
                type.equals(that.type) &&
                (!StringUtils.isNotBlank(storageId) || storageId.equals(that.storageId)) &&
                repositoryId.equals(that.repositoryId);
    }

    @Override
    public int hashCode() {
        return StringUtils.isNotBlank(storageId) ? Objects.hash(node, type, storageId, repositoryId) : Objects.hash(node, type, repositoryId);
    }
}