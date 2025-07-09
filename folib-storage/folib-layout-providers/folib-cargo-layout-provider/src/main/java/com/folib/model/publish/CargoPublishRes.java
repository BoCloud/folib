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
package com.folib.model.publish;

import com.folib.utils.CollectionUtils;

import java.util.List;

public class CargoPublishRes {

    private CargoPublishWarnings warnings;


    public CargoPublishRes() {
    }


    public CargoPublishWarnings getWarnings() {
        return this.warnings;
    }

    public CargoPublishRes(List<String> invalidCategories, List<String> invalidBadges, List<String> other) {
        this.warnings = new CargoPublishWarnings(invalidCategories, invalidBadges, other);
    }

    public boolean warningsAbsent() {
        return (CollectionUtils.isNullOrEmpty(this.warnings.invalidCategories) &&
                CollectionUtils.isNullOrEmpty(this.warnings.invalidBadges) &&
                CollectionUtils.isNullOrEmpty(this.warnings.other));
    }

    public static class CargoPublishWarnings {
        private List<String> invalidCategories;

        private List<String> invalidBadges;

        private List<String> other;


        private CargoPublishWarnings() {
        }

        public CargoPublishWarnings(List<String> invalidCategories, List<String> invalidBadges, List<String> other) {
            this.invalidCategories = invalidCategories;
            this.invalidBadges = invalidBadges;
            this.other = other;
        }


        public List<String> getInvalidCategories() {
            return this.invalidCategories;
        }

        public List<String> getInvalidBadges() {
            return this.invalidBadges;
        }

        public List<String> getOther() {
            return this.other;
        }
    }
}
