package com.veadan.folib.domain.adapter.jfrog;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DockHistory {

    /**
     * 创建时间
     */
    private String created;

    /**
     * 作者
     */
    private String createdBy;

    /**
     * 备注
     */
    private String comment;

    /**
     * 是否是空层 true 是 false 不是
     */
    private Boolean emptyLayer;
}
