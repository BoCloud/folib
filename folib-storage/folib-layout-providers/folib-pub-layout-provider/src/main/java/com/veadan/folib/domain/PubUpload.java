package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * @author leipenghui
 * @date 2024/6/13
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PubUpload {

    /**
     * 上传地址
     */
    private String url;

    /**
     * 字段
     */
    private Map<String, String> fields;
}
