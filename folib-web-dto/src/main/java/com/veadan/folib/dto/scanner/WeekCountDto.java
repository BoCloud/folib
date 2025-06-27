package com.veadan.folib.dto.scanner;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2022/12/28
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class WeekCountDto {

    /**
     * 日期
     */
    private List<WeekDayCountDto> dayCountList;

    /**
     * 对比数据
     */
    private CompareCountDto compareCount;
}
