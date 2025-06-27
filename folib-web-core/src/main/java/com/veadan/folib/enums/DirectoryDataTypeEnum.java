package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 目录数据类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum DirectoryDataTypeEnum {

    /**
     * 仓库
     */
    REPOSITORY(1),

    /**
     * 回收站
     */
    TRASH(2),

    /**
     * 存储空间
     */
    STORAGE(3),

    /**
     * 平台
     */
    PLATFORM(4),

    /**
     * 存储设备
     */
    STORAGE_DEVICE(5),
    ;

    /**
     * type
     */
    private Integer type;

}
