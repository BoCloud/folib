package com.veadan.folib.php;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2022/12/14
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PhpSearchResult {

    /**
     * 结果列表
     */
    private List<PhpSearchPackage> results;

    /**
     * 总数
     */
    private Integer total;

}
