package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class StatusInfo {

    /**
     * 总数
     */
    private Integer total;

    /**
     * 成功数量
     */
    private Integer success;

    /**
     * 失败数量
     */
    private Integer fail;
}
