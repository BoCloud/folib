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
package com.folib.domain;

import java.util.regex.Pattern;

public class RpmNamingPatterns
{
    private static final String RPM_PACKAGE_NAME_REGEXP = "^([a-zA-Z0-9_.\\-+]+?)(?=-\\d)";

    private static final String RPM_PACKAGE_VERSION_REGEXP = "(?<=-)([\\d\\.]+)(?=-)";

    private static final String RPM_PACKAGE_RELEASE_REGEXP = ".*-([a-zA-Z0-9_.\\-\\+]*)\\..*\\.rpm";

    private static final String RPM_PACKAGE_TYPE_REGEXP = "(src)(?=(\\.rpm$))";

    private static final String RPM_PACKAGE_ARCH_REGEXP = "(i386|i686|alpha|sparc|mips|ppc|pcc|m68k|SGI|ppc64le|src|s390x|aarch64|x86_64|noarch)(?=(\\.rpm$))";

    private static final String RPM_PACKAGE_EXTENSION_REGEXP = "(\\.rpm)(?=$)";



    public static final Pattern RPM_PACKAGE_NAME_REGEXP_PATTERN = Pattern.compile(RPM_PACKAGE_NAME_REGEXP);

    public static final Pattern RPM_PACKAGE_VERSION_REGEXP_PATTERN = Pattern.compile(RPM_PACKAGE_VERSION_REGEXP);

    public static final Pattern RPM_PACKAGE_RELEASE_REGEXP_PATTERN = Pattern.compile(RPM_PACKAGE_RELEASE_REGEXP);

    public static final Pattern RPM_PACKAGE_TYPE_REGEXP_PATTERN = Pattern.compile(RPM_PACKAGE_TYPE_REGEXP);

    public static final Pattern RPM_PACKAGE_ARCH_REGEXP_PATTERN = Pattern.compile(RPM_PACKAGE_ARCH_REGEXP);

    public static final Pattern RPM_PACKAGE_EXT_REGEXP_PATTERN = Pattern.compile(RPM_PACKAGE_EXTENSION_REGEXP);
}
