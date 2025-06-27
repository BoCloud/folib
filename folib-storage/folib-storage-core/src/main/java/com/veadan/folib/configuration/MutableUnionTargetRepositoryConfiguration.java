package com.veadan.folib.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.util.Objects;

/**
 * @author veadan
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
     * 类型
     */
    private String type = "inner";

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
                type.equals(that.type) &&
                (!StringUtils.isNotBlank(storageId) || storageId.equals(that.storageId)) &&
                repositoryId.equals(that.repositoryId);
    }

    @Override
    public int hashCode() {
        return StringUtils.isNotBlank(storageId) ? Objects.hash(node, type, storageId, repositoryId) : Objects.hash(node, type, repositoryId);
    }
}