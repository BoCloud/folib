package com.veadan.folib.enums;

import com.google.common.collect.Lists;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.Collections;
import java.util.List;

/**
 * 制品仓库类型枚举
 *
 * @author leipenghui
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum VersionConditionTypeEnum {

    /**
     * 小于
     */
    LT("<", Lists.newArrayList(-1)),
    /**
     * 小于等于
     */
    LE("<=", Lists.newArrayList(-1, 0)),
    /**
     * 等于
     */
    EQ("=", Lists.newArrayList(0)),
    ;

    /**
     * name
     */
    private String condition;
    /**
     * value
     */
    private List<Integer> value;

    public static List<Integer> queryValue(String condition) {
        for (VersionConditionTypeEnum item : VersionConditionTypeEnum.values()) {
            if (item.condition.equals(condition)) {
                return item.value;
            }
        }
        return Collections.emptyList();
    }

}
