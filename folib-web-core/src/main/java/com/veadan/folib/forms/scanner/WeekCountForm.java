package com.veadan.folib.forms.scanner;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2022/12/28
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class WeekCountForm {

    /**
     * 日期
     */
    private List<WeekDayCountForm> dayCountList;

    /**
     * 对比数据
     */
    private CompareCountForm compareCount;
}
