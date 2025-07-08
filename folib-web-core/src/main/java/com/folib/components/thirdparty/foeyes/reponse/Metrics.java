package com.folib.components.thirdparty.foeyes.reponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/4/24
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class Metrics {

    /**
     * critical
     */
    private Integer critical;

    /**
     * high
     */
    private Integer high;

    /**
     * medium
     */
    private Integer medium;

    /**
     * low
     */
    private Integer low;

    /**
     * unassigned
     */
    private Integer unassigned;

    /**
     * vulnerabilities
     */
    private Integer vulnerabilities;

    /**
     * vulnerableComponents
     */
    private Integer vulnerableComponents;

    /**
     * components
     */
    private Integer components;

    /**
     * suppressed
     */
    private Integer suppressed;

    /**
     * inheritedRiskScore
     */
    private Double inheritedRiskScore;
}
