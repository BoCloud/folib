package com.veadan.folib.dto.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 远程仓库状态vo
 *
 * @author leipenghui
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RepositoryAliveDto {

    /**
     * 状态码
     */
    private Integer statusCode;

    /**
     * 是否存活
     */
    private Boolean alive;

}
