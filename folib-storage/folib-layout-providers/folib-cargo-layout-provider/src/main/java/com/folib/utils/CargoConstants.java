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
package com.folib.utils;

public interface CargoConstants {
    public static final String CARGO_FILE = "Cargo.toml";

    public static final String DOWNLOAD_PATH = "v1/crates/";

    public static final String DOWNLOAD_PATH_SUFFIX = "/download";

    public static final String CRATE_SUFFIX = ".crate";

    public static final String CONFIG_FILE = "config.json";

    public static final String INDEX_PATH = "index/";

    public static final String ORIGINAL_CONFIG_FILE = "config.original.json";

    public static final String LONG_METADATA_FILE_SUFFIX = ".json";

    public static final String METADATA_DIRECTORY = ".cargo";

    public static final String PROPERTY_PREFIX = "crate.";

    public static final String CRATE_NAME = "name";

    public static final String CRATE_VERSION = "version";

    public static final String CRATE_AUTHORS = "authors";

    public static final String CRATE_DESCRIPTION = "description";

    public static final String CRATE_DOCUMENTATION = "documentation";

    public static final String CRATE_HOMEPAGE = "homepage";

    public static final String CRATE_REPOSITORY = "repository";

    public static final String CRATE_LICENSE = "license";

    public static final String CRATE_LICENSE_FILE = "license-file";

    public static final String CRATE_KEYWORDS = "keywords";

    public static final String CRATE_CATEGORIES = "categories";

    public static final String CRATE_DEPENDENCIES = "dependencies";

    public static final String CRATE_DEV_DEPENDENCIES = "dev-dependencies";

    public static final String CRATE_BUILD_DEPENDENCIES = "build-dependencies";

    public static final String CRATE_FEATURES = "features";

    public static final String YANKED = "crate.yanked";

    public static final String CRATE_VERSION_REQ = "versionReq";

    public static final String CRATE_OPTIONAL = "optional";

    public static final String CRATE_DEFAULT_FEATURES = "defaultFeatures";

    public static final String CRATE_TARGET = "target";

    public static final String CRATE_KIND = "kind";

    public static final String CRATE_REGISTRY = "registry";

    public static final String CRATE_REGISTRY_INDEX = "registry-index";

    public static final String CRATE_PACKAGE = "package";

    public static final String QUERY = "q";

    public static final String PER_PAGE = "per_page";

    public static final String CARGO_INTERNAL_INDEX_PARAM = "internalIndex";
}
