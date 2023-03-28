package com.veadan.folib.forms.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

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
     * 存储空间
     */
    @NotBlank(message = "存储空间不能为空")
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
                storageId.equals(that.storageId) &&
                repositoryId.equals(that.repositoryId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(node, storageId, repositoryId);
    }
}