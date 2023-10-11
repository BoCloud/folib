package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

/**
 * @author leipenghui
 * @date 2023/10/11
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ArtifactSearchConditionTypeEnum {


    /**
     * match
     */
    MATCH("match"),
    /**
     * nmatch
     */
    N_MATCH("nmatch"),
    /**
     * EQ
     */
    EQ("eq"),
    /**
     * ne
     */
    NE("ne"),
    ;

    private String type;

    public static ArtifactSearchConditionTypeEnum queryTypeEnum(String value) {
        if (StringUtils.isBlank(value)) {
            return MATCH;
        }
        ArtifactSearchConditionTypeEnum result = null;
        for (ArtifactSearchConditionTypeEnum artifactSearchConditionTypeEnum : ArtifactSearchConditionTypeEnum.values()) {
            if (artifactSearchConditionTypeEnum.getType().equals(value)) {
                result = artifactSearchConditionTypeEnum;
                break;
            }
        }
        if (Objects.isNull(result)) {
            return MATCH;
        }
        return result;
    }
}
