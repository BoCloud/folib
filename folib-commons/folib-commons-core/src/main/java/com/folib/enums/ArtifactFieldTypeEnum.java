package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

/**
 * @author veadan
 * @date 2023/12/25
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ArtifactFieldTypeEnum {


    /**
     * created
     */
    CREATED("created", "created", "date"),
    /**
     * lastUpdated
     */
    UPDATED("updated", "lastUpdated", "date"),
    /**
     * lastUpdated
     */
    MODIFIED("modified", "lastUpdated", "date"),
    /**
     * artifactPath
     */
    PATH("path", "artifactPath", "string"),
    ;

    private String artifactory;

    private String folibary;

    private String type;

    public static ArtifactFieldTypeEnum queryTypeEnum(String artifactory) {
        if (StringUtils.isBlank(artifactory)) {
            return null;
        }
        ArtifactFieldTypeEnum artifactFieldTypeEnum = null;
        for (ArtifactFieldTypeEnum artifactSearchConditionTypeEnum : ArtifactFieldTypeEnum.values()) {
            if (artifactSearchConditionTypeEnum.getArtifactory().equals(artifactory)) {
                artifactFieldTypeEnum = artifactSearchConditionTypeEnum;
                break;
            }
        }
        return artifactFieldTypeEnum;
    }
}
