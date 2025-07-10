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
package com.folib.model.response;

import cn.hutool.core.lang.Assert;
import com.folib.enums.BusinessCodeEnum;
import lombok.Data;

import java.io.Serializable;

/**
 * @author veadan
 * @date 2023/12/19 16:31
 */
@Data
public class Result<T> implements Serializable {
    private static final long serialVersionUID = 1L;
    private static Integer SUCCESS_CODE = BusinessCodeEnum.SUCCESS.getCode();
    private static Integer ERROR_CODE = BusinessCodeEnum.INTERNAL_SERVER_ERROR.getCode();
    private boolean success;
    private String message;
    private Integer code;
    private long timestamp = System.currentTimeMillis();
    private T data;

    public static <T> Result<T> success(T data) {
        return buildResult(true, BusinessCodeEnum.SUCCESS.getCode(), BusinessCodeEnum.SUCCESS.getMessage(), data);
    }

    public static <T> Result<T> success() {
        return buildResult(true, BusinessCodeEnum.SUCCESS.getCode(), BusinessCodeEnum.SUCCESS.getMessage(), (T) null);
    }

    public static <T> Result<T> error(Integer code, String message) {
        Assert.isTrue(!SUCCESS_CODE.equals(code), "code 必须是错误的！", new Object[0]);
        return buildResult(false, code, message, (T) null);
    }

    public static <T> Result<T> error(Result<?> result) {
        return error(result.getCode(), result.getMessage());
    }

    public static <T> Result<T> error(String message) {
        return buildResult(false, ERROR_CODE, message, (T) null);
    }

    public static <T> Result<T> error(BusinessCodeEnum errorCode) {
        return buildResult(false, errorCode.getCode(), errorCode.getMessage(), (T) null);
    }

    public static <T> Result<T> error(Exception exception) {
        return buildResult(false, BusinessCodeEnum.INTERNAL_SERVER_ERROR.getCode(), exception.getMessage(), (T) null);
    }

    private static <T> Result<T> buildResult(boolean success, Integer code, String msg, T obj) {
        Result<T> r = new Result();
        r.setSuccess(success);
        r.setCode(code);
        r.setMessage(msg);
        r.setData(obj);
        return r;
    }

    public Result() {
    }
}
