package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 业务代码定义
 *
 * @author veadan
 * @date 2023/12/19 16:34
 */
@Getter
@AllArgsConstructor
public enum BusinessCodeEnum {
    SUCCESS(20000, "操作成功"),
    BAD_REQUEST(40000, "请求参数不正确"),
    INTERNAL_SERVER_ERROR(50000, "服务器未知异常"),


    ARTIFACT_SLICE_UPLOAD_CHUNK_FILE_SAVE_FAILED(51000, "切片文件转存失败"),
    ARTIFACT_SLICE_UPLOAD_CHUNK_FILE_UPLOAD_FAILED(51001, "第%s切片文件上传失败，请重新上传"),
    ARTIFACT_SLICE_UPLOAD_CHUNK_FILE_MERGE_FAILED(51002, "切片上传文件合并失败"),
    ARTIFACT_SLICE_UPLOAD_MD5_CHECK_FAILED(51003, "切片上传后合并的文件与原文件的MD5不一致"),
    ;
    private int code;
    private String message;

}
