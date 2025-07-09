/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.enums;

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
