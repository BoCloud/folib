package com.veadan.folib.model.response;

import cn.hutool.core.lang.Assert;
import com.veadan.folib.enums.BusinessCodeEnum;
import lombok.Data;

import java.io.Serializable;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/19 16:31
 * @since x.x.x
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
