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
public class TagSet {

    /**
     * uuid
     */
    private String uuid;
    /**
     * 名称
     */
    private String name;
    /**
     * 本地id
     */
    private Long nativeId;
}
