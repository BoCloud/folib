package com.veadan.folib.components.thirdparty.foeyes.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/4/22
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum UploadStatusEnum {

    /**
     * 待上传
     */
    WAIT_UPLOAD("waitUpload"),
    /**
     * 上传失败
     */
    UPLOAD_FAIL("uploadFail"),
    /**
     * 上传成功
     */
    UPLOAD_SUCCESS("uploadSuccess"),
    ;

    /**
     * type
     */
    private String type;
}
