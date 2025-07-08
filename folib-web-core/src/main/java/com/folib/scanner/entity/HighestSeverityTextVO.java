package com.folib.scanner.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author veadan
 * @date 2022/9/8
 **/
@Data
@Accessors(chain = true)
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class HighestSeverityTextVO implements Serializable {

    /**
     * 最高漏洞等级
     */
    private String highestSeverityText;
}
