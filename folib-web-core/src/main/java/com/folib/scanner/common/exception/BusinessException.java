package com.folib.scanner.common.exception;

import com.folib.enums.BusinessCodeEnum;

/**
 * 业务异常基础类
 * @author Veadan
 * @version 2018/1/13.
 */
public class BusinessException extends BaseException {
    public BusinessException(String message) {
        super(message, BusinessCodeEnum.INTERNAL_SERVER_ERROR.getCode());
    }

    public BusinessException(BusinessCodeEnum businessCodeEnum) {
        super(businessCodeEnum.getMessage(), businessCodeEnum.getCode());
    }
    
    public BusinessException(BusinessCodeEnum businessCodeEnum, String ... messageFillParam) {
        super(String.format(businessCodeEnum.getMessage(), messageFillParam), businessCodeEnum.getCode());
    }
    
    public BusinessException(String message, Integer code) {
        super(message, code);
    }
}
