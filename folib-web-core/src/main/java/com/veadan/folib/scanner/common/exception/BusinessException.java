package com.veadan.folib.scanner.common.exception;

import org.springframework.http.HttpStatus;

/**
 * 业务异常基础类
 * @author Veadan
 * @version 2018/1/13.
 */
public class BusinessException extends BaseException {
    public BusinessException(String message) {
        super(message, HttpStatus.NOT_MODIFIED.value());
    }
}
