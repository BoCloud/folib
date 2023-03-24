package com.veadan.folib.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.annotation.concurrent.Immutable;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author leipenghui
 */
@Immutable
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UnionTargetRepositoryConfiguration
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
        if (!(o instanceof UnionTargetRepositoryConfiguration)) {
            return false;
        }
        UnionTargetRepositoryConfiguration that = (UnionTargetRepositoryConfiguration) o;
        return node.equals(that.node) &&
                storageId.equals(that.storageId) &&
                repositoryId.equals(that.repositoryId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(node, storageId, repositoryId);
    }
}