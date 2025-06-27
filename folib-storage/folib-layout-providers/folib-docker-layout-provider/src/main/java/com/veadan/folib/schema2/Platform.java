package com.veadan.folib.schema2;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Platform {

    /**
     * 基础架构
     */
    private String architecture;
    /**
     * 镜像OS
     */
    private String os;

    /**
     * variant
     */
    private String variant;
}
