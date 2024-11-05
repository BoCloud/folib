package com.veadan.folib.controllers.unicom;

import lombok.Data;

import java.util.Set;

/**
 * @author huayanjun
 * @since 2024-10-29 14:11
 */
@Data
public class UnicomLayoutSupportVO {

    private boolean supported;

    private Set<String> types;

}
