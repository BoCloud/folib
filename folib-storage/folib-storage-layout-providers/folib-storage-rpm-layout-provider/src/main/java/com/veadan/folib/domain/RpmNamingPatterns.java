package com.veadan.folib.domain;

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
