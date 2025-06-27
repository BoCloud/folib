package com.veadan.folib.domain.bom;

import com.alibaba.fastjson.JSONObject;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/4/23
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class Bom {

    /**
     * bomId
     */
    private String bomId;

    /**
     * bomValue
     */
    private JSONObject bomValue;

    /**
     * foEyes
     */
    private FoEyes foEyes;
}
