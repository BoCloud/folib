package com.veadan.folib.forms.scanner;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2022/12/28
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DayCountForm {

    /**
     * 日期
     */
    private String date;

    /**
     * 依赖数量
     */
    private Long dependencyCount;

    /**
     * 漏洞数量
     */
    private Long vulnerabilitiesCount;
}
