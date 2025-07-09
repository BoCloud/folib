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
package com.folib.constants;

import com.google.common.collect.Sets;
import com.folib.domain.PypiPackageInfo;

import java.util.Set;
import java.util.regex.Pattern;

/**
 * @author veadan
 * @date 2024/7/2
 **/
public interface PypiConstants {

    String PYPI_API = "pypi";

    String INDEX_DIR = ".pypi";

    String SIMPLE_INDEX = ".pypi/simple.html";

    String PROP_REQUIRED_PYTHON = "pypi.requires.python";

    String PROP_YANKED = "pypi.yanked";

    String PROP_METADATA_FILE_HASH = "pypi.metadata.file.hash";

    String PKG_INFO = "PKG-INFO";

    String METADATA = "METADATA";

    String PROP_NAME = "pypi.name";

    String PROP_NORMALIZED_NAME = "pypi.normalized.name";

    String PROP_VERSION = "pypi.version";

    String PROP_SUMMARY = "pypi.summary";

    String PROP_LICENSE = "pypi.license";

    String PACKAGE_INDEX_PREFIX = "../..";

    Set<String> LOCALLY_GENERATED_PROPERTIES = Set.of("pypi.name", "pypi.normalized.name", "pypi.version", "pypi.summary", "pypi.requires.python", "pypi.metadata.file.hash");

    Set<String> PYPI_EXTENSIONS = Sets.newHashSet("exe", "tar.gz", "bz2", "rpm", "deb", "zip", "tgz", "egg", "dmg", "msi",
            "whl");

    String DEFAULT_PYPI_REGISTRY = "https://pypi.org";

    String TEST_PYPI_REGISTRY = "https://test.pypi.org";

    int LIMIT_RESULTS_SIZE = 10000;

    String NAME_SEARCH_PARAMETER = "name";

    String SUMMARY_SEARCH_PARAMETER = "summary";

    String DEFAULT_SUMMARY_VALUE = "UNKNOWN";

    String PYPI_URL_DELIMITER = "#";

    String PYPI_SEARCH_ERROR_RESPONSE = "<methodResponse>\n    <fault>\n        <value>\n            <struct>\n                <member>\n                    <name>faultCode</name>\n                    <value>\n                        <int>1</int>\n                    </value>\n                </member>\n                <member>\n                    <name>faultString</name>\n                    <value>\n                        <string>No results found for query</string>\n                    </value>\n                </member>\n            </struct>\n        </value>\n    </fault>\n</methodResponse>\n";

    String PYPI_EMPTY_SEARCH_RESPONSE = "<methodResponse>\n    <params>\n        <param>\n            <value>\n                <array>\n                    <data/>\n                </array>\n            </value>\n        </param>\n    </params>\n</methodResponse>";

    Pattern PACKAGE_NAME_PATTERN = Pattern.compile(PypiPackageInfo.NAME_FORMAT);

    String PYPI_SIMPLE = "/simple";

    String PYPI_PACKAGES = "/packages";

    /**
     * pypi 包索引文件后缀
     */
    String PACKAGE_HTML_EXTENSION = ".html";
}