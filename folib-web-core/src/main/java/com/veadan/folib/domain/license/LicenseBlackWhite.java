package com.veadan.folib.domain.license;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * @author veadan
 * @date 2024/10/14
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class LicenseBlackWhite {

    /**
     * licenseId
     */
    @NotBlank(message = "请传入licenseId")
    private String licenseId;

    /**
     * 黑白名单类型 0 无状态 1 白名单 2 黑名单
     */
    @NotNull(message = "请传入licenseId")
    private Integer blackWhiteType;

}
