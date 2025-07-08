package com.folib.domain.client;

import com.folib.enums.ResponseDataTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.ws.rs.core.MultivaluedMap;

/**
 * @author veadan
 * @date 2024/1/25
 **/
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
public class ResponseResult {

    /**
     * 状态码
     */
    private Integer httpStatus;

    /**
     * 数据
     */
    private String data;

    /**
     * 数据类型
     */
    private ResponseDataTypeEnum dataType;

    /**
     * 头信息
     */
    private MultivaluedMap<String, String> headers;
}
