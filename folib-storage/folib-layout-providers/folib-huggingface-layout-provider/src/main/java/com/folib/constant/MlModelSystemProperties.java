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
package com.folib.constant;

import lombok.Generated;

public final class MlModelSystemProperties {
    @Generated
    private MlModelSystemProperties() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    public static final SystemProperty<Integer> ML_MODEL_METADATA_PACKAGES_INDEX_WORKERS = SystemProperty.of("huggingfaceml.metadata.packages.index.workers", Integer.valueOf(5));

    public static final SystemProperty<Integer> ML_MODEL_METADATA_INDEX_WORKERS = SystemProperty.of("huggingfaceml.metadata.calculation.workers", Integer.valueOf(5));

    public static final SystemProperty<Integer> ML_MODEL_CONCURRENT_UPLOADS = SystemProperty.of("huggingfaceml.concurrent.commits.limit", Integer.valueOf(3));

    public static final SystemProperty<Long> ML_MODEL_LFS_FILE_MIN_SIZE = SystemProperty.of("huggingfaceml.lfs.min.size", Long.valueOf(10485760L));
}
