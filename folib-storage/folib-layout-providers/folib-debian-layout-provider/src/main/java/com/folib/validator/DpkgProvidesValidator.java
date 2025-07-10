/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.validator;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;

/**
 * @author veadan
 * @since 2024-09-03 13:49
 */
@Slf4j
public class DpkgProvidesValidator implements DpkgPackageMetadataValidator {
    public void validate(String key, String... values) throws MetadataValidationException {
        log.debug("Starting to validate metadata");
        if (StringUtils.isBlank(key)) {
            log.debug("Provides line is empty. quitting the validator");
        } else if (values != null && values.length != 0) {
            for (String value : values) {
                if (value.isEmpty()) {
                    String msg = "The Control file contains a malformed Provides line; one of the packages is empty";
                    log.error(msg);
                    throw new MetadataValidationException(msg);
                }
            }
        } else {
            String msg = "The Control file contains an empty field: Provides";
            log.error(msg);
            throw new MetadataValidationException(msg);
        }
    }
}
