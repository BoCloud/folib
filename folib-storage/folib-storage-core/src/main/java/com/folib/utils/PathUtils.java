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

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

/**
 * @author veadan
 * @since 2025-02-26 11:18
 */
public class PathUtils {
    private static final Pattern PATTERN_SLASHES = Pattern.compile("/+");

    public PathUtils() {
    }

    public static boolean hasLength(String str) {
        return str != null && str.length() > 0;
    }

    public static boolean hasText(String str) {
        if (!hasLength(str)) {
            return false;
        } else {
            int strLen = str.length();

            for(int i = 0; i < strLen; ++i) {
                if (!Character.isWhitespace(str.charAt(i))) {
                    return true;
                }
            }

            return false;
        }
    }

    public static boolean isFolderPath(String path) {
        return !hasLength(path) ? true : path.endsWith("/");
    }

    public static String trimWhitespace(String str) {
        if (!hasLength(str)) {
            return str;
        } else {
            StringBuffer buf = new StringBuffer(str);

            while(buf.length() > 0 && Character.isWhitespace(buf.charAt(0))) {
                buf.deleteCharAt(0);
            }

            while(buf.length() > 0 && Character.isWhitespace(buf.charAt(buf.length() - 1))) {
                buf.deleteCharAt(buf.length() - 1);
            }

            return buf.toString();
        }
    }

    public static String getFileName(String path) {
        if (path == null) {
            return null;
        } else {
            File dummy = new File(path);
            return dummy.getName();
        }
    }

    public static String getFilePath(String fullFileName, String fileNameOnly) {
        return fullFileName != null && fileNameOnly != null ? fullFileName.replace(fileNameOnly, "") : null;
    }

    public static String getParent(String path) {
        if (path == null) {
            return null;
        } else {
            File dummy = new File(path);
            return formatPath(dummy.getParent());
        }
    }

    public static String getExtension(String path) {
        if (path == null) {
            return null;
        } else {
            int dotPos = path.lastIndexOf(46);
            return dotPos < 0 ? null : path.substring(dotPos + 1);
        }
    }

    public static String stripExtension(String path) {
        String result = path;
        String extension = getExtension(path);
        if (extension != null) {
            result = path.substring(0, path.length() - extension.length() - 1);
        }

        return result;
    }

    public static String collectionToDelimitedString(Iterable<String> iterable) {
        return collectionToDelimitedString(iterable, ",");
    }

    public static String collectionToDelimitedString(Iterable<String> iterable, String delim) {
        if (iterable == null) {
            return "";
        } else {
            Iterator<String> it = iterable.iterator();
            if (it != null && it.hasNext()) {
                StringBuilder sb = new StringBuilder();

                while(it.hasNext()) {
                    String next = (String)it.next();
                    String str = next != null ? next : null;
                    if (str != null) {
                        str = str.trim();
                        if (str.length() != 0) {
                            sb.append(str);
                            if (it.hasNext()) {
                                sb.append(delim);
                            }
                        }
                    }
                }

                return sb.toString();
            } else {
                return "";
            }
        }
    }

    public static List<String> includesExcludesPatternToStringList(String str) {
        return delimitedListToStringList(str, ",", "\r\n\f ");
    }

    public static List<String> delimitedListToStringList(String str, String delimiter) {
        return delimitedListToStringList(str, delimiter, "\r\n\f\t ");
    }

    public static List<String> delimitedListToStringList(String str, String delimiter, String charsToDelete) {
        List<String> result = new ArrayList();
        if (str == null) {
            return result;
        } else if (delimiter == null) {
            result.add(str);
            return result;
        } else {
            if ("".equals(delimiter)) {
                for(int i = 0; i < str.length(); ++i) {
                    result.add(deleteAny(str.substring(i, i + 1), charsToDelete));
                }
            } else {
                int delPos;
                int pos;
                for(pos = 0; (delPos = str.indexOf(delimiter, pos)) != -1; pos = delPos + delimiter.length()) {
                    result.add(deleteAny(str.substring(pos, delPos), charsToDelete));
                }

                if (str.length() > 0 && pos <= str.length()) {
                    result.add(deleteAny(str.substring(pos), charsToDelete));
                }
            }

            return result;
        }
    }

    public static String deleteAny(String inString, String charsToDelete) {
        if (hasLength(inString) && hasLength(charsToDelete)) {
            StringBuffer out = new StringBuffer();

            for(int i = 0; i < inString.length(); ++i) {
                char c = inString.charAt(i);
                if (charsToDelete.indexOf(c) == -1) {
                    out.append(c);
                }
            }

            return out.toString();
        } else {
            return inString;
        }
    }

    public static String formatRelativePath(String path) {
        path = formatPath(path);
        return trimSlashes(path);
    }

    public static String formatPath(String path) {
        if (hasText(path)) {
            path = path.replace('\\', '/');
            return normalizeSlashes(path).toString();
        } else {
            return "";
        }
    }

    public static String trimSlashes(CharSequence path) {
        if (path == null) {
            return null;
        } else {
            path = trimLeadingSlashChars(path);
            path = trimTrailingSlashesChars(path);
            return path.toString();
        }
    }

    public static String trimLeadingSlashes(CharSequence path) {
        CharSequence res = trimLeadingSlashChars(path);
        return res != null ? res.toString() : null;
    }

    public static CharSequence normalizeSlashes(CharSequence path) {
        return path == null ? null : PATTERN_SLASHES.matcher(path).replaceAll("/");
    }

    public static CharSequence trimLeadingSlashChars(CharSequence path) {
        if (path == null) {
            return null;
        } else if (path.length() > 0 && path.charAt(0) == '/') {
            path = path.subSequence(1, path.length());
            return trimLeadingSlashChars(path);
        } else {
            return path;
        }
    }

    public static String trimTrailingSlashes(CharSequence path) {
        CharSequence res = trimTrailingSlashesChars(path);
        return res != null ? res.toString() : null;
    }

    public static CharSequence trimTrailingSlashesChars(CharSequence path) {
        if (path == null) {
            return null;
        } else if (path.length() > 0 && path.charAt(path.length() - 1) == '/') {
            path = path.subSequence(0, path.length() - 1);
            return trimTrailingSlashes(path);
        } else {
            return path;
        }
    }

    public static String addTrailingSlash(String path) {
        if (path == null) {
            return null;
        } else {
            path = trimWhitespace(path);
            return path.endsWith("/") ? path : path + "/";
        }
    }

    public static String getRelativePath(String parentPath, String childPath) {
        childPath = childPath.substring(parentPath.length(), childPath.length());
        childPath = formatRelativePath(childPath);
        return childPath;
    }

    public static String injectString(String str, String toInject, int injectionIndex) {
        return hasText(str) && hasText(toInject) ? str.substring(0, injectionIndex) + toInject + str.substring(injectionIndex) : str;
    }

    public static String[] getPathElements(String path) {
        if (path == null) {
            return new String[0];
        } else {
            if (path.startsWith("/")) {
                path = path.substring(1);
            }

            return path.split("/");
        }
    }

    public static String getAncesstor(String path, int depth) {
        String[] elements = getPathElements(path);
        if (elements.length < depth) {
            throw new IllegalArgumentException("Ancesstor of level " + depth + " does not exist for " + path + ".");
        } else {
            StringBuilder sb = new StringBuilder();

            for(int i = 0; i < elements.length - depth; ++i) {
                String element = elements[i];
                if (sb.length() > 0) {
                    sb.append('/');
                }

                sb.append(element);
            }

            if (path.startsWith("/")) {
                sb.insert(0, '/');
            }

            return sb.toString();
        }
    }

    public static String getFirstPathElement(String path) {
        if (path == null) {
            return null;
        } else {
            String[] elements = getPathElements(path);
            return elements.length > 0 ? elements[0] : "";
        }
    }

    public static String getLastPathElement(String path) {
        if (path == null) {
            return null;
        } else {
            String[] elements = getPathElements(path);
            return elements.length > 0 ? elements[elements.length - 1] : "";
        }
    }

    public static String stripFirstPathElement(String path) {
        if (path == null) {
            return null;
        } else {
            path = trimLeadingSlashes(path);
            int indexOfFirstSlash = path.indexOf(47);
            return indexOfFirstSlash < 0 ? "" : path.substring(indexOfFirstSlash + 1);
        }
    }
}
