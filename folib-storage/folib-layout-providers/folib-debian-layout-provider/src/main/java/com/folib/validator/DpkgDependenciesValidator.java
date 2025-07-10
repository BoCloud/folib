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

import java.util.regex.Pattern;

/**
 * @author veadan
 * @since 2024-09-03 13:55
 */
@Slf4j
public class DpkgDependenciesValidator  implements DpkgPackageMetadataValidator{
    private static final Pattern PACKAGE_NAME_VALIDATION_REGEX = Pattern.compile("[a-z0-9+-.]+");
    private boolean emptyFieldValidation;

    public DpkgDependenciesValidator(boolean emptyFieldValidation) {
        this.emptyFieldValidation = emptyFieldValidation;
    }

    public void validate(String key, String... values) throws MetadataValidationException {
        log.debug("Starting to validate metadata");
        if (StringUtils.isBlank(key)) {
            log.debug("Depends line is empty. quitting the validator");
        } else if (!this.emptyFieldValidation || values != null && values.length != 0) {
            if (values != null) {
                for (String dependency : values) {
                    String dependencyAfterParsing = dependency.trim().replaceAll(" +", " ");
                    if (dependencyAfterParsing.isEmpty()) {
                        throw new MetadataValidationException("'Depends' field missing package name where package name expected");
                    }
                    for(int i = 0; i < dependencyAfterParsing.length(); ++i) {
                        char c = dependencyAfterParsing.charAt(i);
                        if (c == ' ') {
                            String charBefore = dependencyAfterParsing.substring(i - 1, i);
                            String charAfter = dependencyAfterParsing.substring(i + 1, i + 2);
                            if (PACKAGE_NAME_VALIDATION_REGEX.matcher(charBefore).matches() && PACKAGE_NAME_VALIDATION_REGEX.matcher(charAfter).matches()) {
                                throw new MetadataValidationException("'Depends' field missing package name, or invalid string where package name expected");
                            }
                        }
                    }
                }
            }
        } else {
            throw new MetadataValidationException("'Depends' field exists but has an empty value");
        }
    }
}
