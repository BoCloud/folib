package com.veadan.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2022/11/16
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CodeSnippet {

    /**
     * name
     */
    private String name;

    /**
     * code
     */
    private String code;
}
