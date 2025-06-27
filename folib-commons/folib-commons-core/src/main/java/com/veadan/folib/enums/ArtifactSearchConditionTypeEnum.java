package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

/**
 * @author veadan
 * @date 2023/10/11
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ArtifactSearchConditionTypeEnum {


    /**
     * match
     */
    MATCH("match", "$match"),
    /**
     * nmatch
     */
    N_MATCH("nmatch", "$nmatch"),
    /**
     * EQ
     */
    EQ("eq", "$eq"),
    /**
     * ne
     */
    NE("ne", "$ne"),
    /**
     * gte
     */
    GTE("gte", "$gte"),
    /**
     * lte
     */
    LTE("lte", "$lte"),
    /**
     * or
     */
    OR("or", "$or"),
    /**
     * and
     */
    AND("and", "$and"),
    ;

    private String type;

    private String source;

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

    public static ArtifactSearchConditionTypeEnum queryTypeEnumBySource(String source) {
        if (StringUtils.isBlank(source)) {
            return MATCH;
        }
        ArtifactSearchConditionTypeEnum result = null;
        for (ArtifactSearchConditionTypeEnum artifactSearchConditionTypeEnum : ArtifactSearchConditionTypeEnum.values()) {
            if (artifactSearchConditionTypeEnum.getSource().equals(source)) {
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
