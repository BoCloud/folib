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

import java.util.regex.Pattern;

/**
 * @author veadan
 * @since 2024-08-27 17:16
 */
public class DebianConstant {


    public static final String LAYOUT_NAME = "debian";

    public static final String LAYOUT_ALIAS = "debian";

    public static final String DISTRIBUTION = "distribution";

    public static final String ARCHITECTURE = "architecture";

    public static final String COMPONENT = "component";

    public static final String FILENAME = "filename";

    public static final String NAME = "name";

    public static final String DEFAULT_EXTENSION = "deb";

    public static final String ATTR_DISTRIBUTION = "deb.distribution";

    public static final String ATTR_COMPONENT = "deb.component";

    public static final String ATTR_ARCHITECTURE = "deb.architecture";




    public static final String PACKAGE_EXTENSION = "package";

    public static final String PACKAGE_PREFIX = "dists";

    public static final String DEB_PREFIX = "pool";
    public static final String EXTENSION = "extension";

//    public static final String DEBIAN_DEB_REGEX = "^pool/(main|contrib|non-free)(?:-[a-z]+)?/(?:lib[a-z0-9]?|[a-z0-9])/([a-z0-9][a-z0-9+.-]*)/(?:[a-z0-9][a-z0-9+.-]*_)([0-9][0-9a-zA-Z.+:~-]*)(?:-[0-9.+~]*)?(?:_[a-z0-9]+)?\\.(?:deb|udeb)$";
    public static final String DEBIAN_DEB_REGEX = "^pool/([^/]+)/(?:lib[a-z0-9]?|[a-z0-9])/([a-z0-9][a-z0-9+.-]*)/(?:[a-z0-9][a-z0-9+.-]*_)([0-9][0-9a-zA-Z.+:~-]*)(?:-[0-9.+~]*)?(?:_[a-z0-9]+)?\\.(?:deb|udeb)$";
    public static final Pattern PATH_PATTERN = Pattern.compile(DEBIAN_DEB_REGEX);

    public static final Pattern CUSTOM_PATTERN=Pattern.compile(".*/([^/]+)_([^_]+(?:[-+][^_]+)*)_([^_]+)\\.([a-zA-Z0-9]+)$|([^/]+)_([^_]+(?:[-+][^_]+)*)_([^_]+)\\.([a-zA-Z0-9]+)$");

    public static final String DEBIAN_PACKAGE_REGEX="dists/(?<codename>[a-zA-Z0-9\\-_.]+)/(?<component>[a-zA-Z0-9\\-_]+)/binary-(?<architecture>[!-~]+)/(?<filename>Packages(?:\\.(gz|bz2|xz|lzma))?)";

    public static final Pattern PACKAGE_PATTERN = Pattern.compile(DEBIAN_PACKAGE_REGEX);

    public static final Pattern META_PATTERN=Pattern.compile("^(Packages.*|.*\\.deb)$");
    public static final String CONTROL_FILENAME="Filename";
    public static final String CONTROL_SHA256="SHA256";

    public static final String CONTROL_MD5SUM="MD5sum";
    public static final String CONTROL_SIZE="Size";




}
