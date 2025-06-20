package com.veadan.folib.dto.scanner;

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
public class WeekDayCountDto {

    /**
     * 日期
     */
    private String date;

    /**
     * 漏洞数量
     */
    private Long vulnerabilitiesCount;
}
