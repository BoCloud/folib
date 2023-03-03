package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2023/03/01
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum DictTypeEnum {

    /**
     * 上传进度
     */
    UPLOAD_PROCESS("upload_process"),
    ;

    private String type;

}
