package com.veadan.folib.domain;

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
public class ConanInfo {

    /**
     * 配方信息
     */
    private ConanRecipeInfo recipeInfo;

    /**
     * 包数量
     */
    private Integer packageCount;

}
