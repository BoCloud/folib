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
package com.folib.util;

/**
 * @author veadan
 * @date 2024/7/2
 **/

import com.google.common.annotations.VisibleForTesting;
import com.folib.constants.PypiConstants;
import lombok.Generated;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.regex.Pattern;

public final class PypiUtils {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(PypiUtils.class);

    @Generated
    private PypiUtils() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    private static final String[] PYPI_STANDARDIZED_EXTENSIONS = new String[]{".tar.gz", ".tgz", ".whl"};

    private static final Pattern CANONICAL_VERSION_PATTERN = Pattern.compile("^([1-9][0-9]*!)?(0|[1-9][0-9]*)(\\.(0|[1-9][0-9]*))*((a|b|rc)(0|[1-9][0-9]*))?(\\.post(0|[1-9][0-9]*))?(\\.dev(0|[1-9][0-9]*))?$");

    private static final Pattern LOCAL_VERSION_ALLOWED_CHARS = Pattern.compile("[a-zA-Z0-9.]+");

    private static final Pattern NORMALIZE_SEPARATORS_PATTERN = Pattern.compile("[-_.]+");

    private static final Pattern NORMALIZE_SEPARATORS_PATTERN_ALLOWING_FULL_STOP = Pattern.compile("[-_]+");

    public static final String PYPI_METADATA_FILE_SUFFIX = ".metadata";

    public static String getRemotePackageIndexPath(String packageName) {
        return String.join("/", ".pypi", packageName + ".html");
    }

    public static String getPackageIndexPathLocalRepo(String packageName) {
        packageName = normalizePackageName(packageName);
        return String.join("/", ".pypi", packageName, packageName + ".html");
    }

    public static String getScopedPath(@NonNull String user, @NonNull String scope, @NonNull String path) {
        if (user == null) {
            throw new NullPointerException("user is marked non-null but is null");
        }
        if (scope == null) {
            throw new NullPointerException("scope is marked non-null but is null");
        }
        if (path == null) {
            throw new NullPointerException("path is marked non-null but is null");
        }
        return String.join("/", new CharSequence[]{user, scope, path});
    }

    public static String normalizePackageName(@NonNull String name) {
        if (name == null) {
            throw new NullPointerException("name is marked non-null but is null");
        }
        return NORMALIZE_SEPARATORS_PATTERN.matcher(name.toLowerCase()).replaceAll("-");
    }

    public static String normalizeDistributionName(@NonNull String name) {
        if (name == null) {
            throw new NullPointerException("name is marked non-null but is null");
        }
        return NORMALIZE_SEPARATORS_PATTERN_ALLOWING_FULL_STOP.matcher(name).replaceAll("_");
    }

    public static boolean isSupportedExtension(String path) {
        if (".pypi/simple.html".equals(path)) {
            return false;
        }
        return PypiConstants.PYPI_EXTENSIONS.stream().anyMatch(extension -> StringUtils.endsWithIgnoreCase(path, extension));
    }

    public static boolean isMetadataFile(@NonNull String nameOrPath) {
        if (nameOrPath == null) {
            throw new NullPointerException("nameOrPath is marked non-null but is null");
        }
        return nameOrPath.toLowerCase().endsWith(".metadata");
    }

    private static boolean isVersionInCanonicalForm(@NonNull String publicVersion) {
        if (publicVersion == null) {
            throw new NullPointerException("publicVersion is marked non-null but is null");
        }
        return CANONICAL_VERSION_PATTERN.matcher(publicVersion).matches();
    }

    private static boolean isLocalVersionValid(@NonNull String localVersion) {
        if (localVersion == null) {
            throw new NullPointerException("localVersion is marked non-null but is null");
        }
        return (LOCAL_VERSION_ALLOWED_CHARS.matcher(localVersion).matches() && !localVersion.startsWith(".") &&
                !localVersion.endsWith("."));
    }

    @VisibleForTesting
    static boolean isVersionValid(@NonNull String version) {
        if (version == null) {
            throw new NullPointerException("version is marked non-null but is null");
        }
        String[] versionParts = version.split("\\+");
        if (versionParts.length > 2) {
            return false;
        }
        boolean isLocalSegmentValid = true;
        if (versionParts.length == 2) {
            isLocalSegmentValid = isLocalVersionValid(versionParts[1]);
        }
        return (isLocalSegmentValid && isVersionInCanonicalForm(versionParts[0]));
    }

    @VisibleForTesting
    static String stripExtension(@NonNull String fileName) {
        if (fileName == null) {
            throw new NullPointerException("fileName is marked non-null but is null");
        }
        for (String extension : PypiConstants.PYPI_EXTENSIONS) {
            if (fileName.endsWith("." + extension)) {
                return fileName.substring(0, fileName.lastIndexOf("." + extension));
            }
        }
        log.trace("no pypi extension found in file name: {}", fileName);
        return fileName;
    }

    public static String resolveUrl(String baseUrl, String href) {
        if (href.startsWith("http://") || href.startsWith("https://")) {
            // Absolute URL, no need to resolve
            return href;
        }
        String resolvedUrl = baseUrl + href;
        while (resolvedUrl.contains("../")) {
            int index = resolvedUrl.indexOf("../");
            int slashIndex = resolvedUrl.lastIndexOf('/', index - 2);
            if (slashIndex != -1) {
                resolvedUrl = resolvedUrl.substring(0, slashIndex + 1) + resolvedUrl.substring(index + 3);
            } else {
                // Invalid URL, cannot resolve further
                break;
            }
        }
        return resolvedUrl;
    }

    public static String escapeSpecialCharacters(String packageName) {
        // https://www.python.org/dev/peps/pep-0427/#escaping-and-unicode
        return packageName.replaceAll("[^A-Za-z0-9 ]", "_");
    }
}

