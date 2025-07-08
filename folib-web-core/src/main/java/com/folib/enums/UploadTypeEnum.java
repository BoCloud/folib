package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 上传文件类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum UploadTypeEnum {

    /**
     * 镜像
     */
    IMAGE("image"),
    /**
     * 附属文件
     */
    SUBSIDIARY("subsidiary"),
    ;

    /**
     * type
     */
    private String type;

}
