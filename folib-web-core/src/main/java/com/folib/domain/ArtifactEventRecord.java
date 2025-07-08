package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Objects;

/**
 * @author veadan
 * @date 2022/12/28
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactEventRecord {

    /**
     * 存储空间
     */
    private String storageId;
    /**
     * 仓库名称
     */
    private String repositoryId;
    /**
     * 制品路径
     */
    private String artifactPath;
    /**
     * 事件类型
     */
    private Integer eventType;

    /**
     * path
     */
    private String path;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ArtifactEventRecord)) {
            return false;
        }
        ArtifactEventRecord that = (ArtifactEventRecord) o;
        return storageId.equals(that.storageId) &&
                repositoryId.equals(that.repositoryId) &&
                artifactPath.equals(that.artifactPath) &&
                eventType.equals(that.eventType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(storageId, repositoryId, artifactPath, eventType);
    }
}
