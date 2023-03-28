package com.veadan.folib.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Objects;

/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MutableUnionTargetRepositoryConfiguration
        implements Serializable {

    /**
     * 节点
     */
    private String node;

    /**
     * 存储空间
     */
    private String storageId;

    /**
     * 仓库名称
     */
    private String repositoryId;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof MutableUnionTargetRepositoryConfiguration)) {
            return false;
        }
        MutableUnionTargetRepositoryConfiguration that = (MutableUnionTargetRepositoryConfiguration) o;
        return node.equals(that.node) &&
                storageId.equals(that.storageId) &&
                repositoryId.equals(that.repositoryId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(node, storageId, repositoryId);
    }
}