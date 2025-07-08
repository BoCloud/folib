package com.folib.util;


import org.apache.commons.lang.StringUtils;

public class HelmVersionUtil {
    public static final String PATTERN_TO_MARK_VERSION = "___VERSIONPROP___";

    static String addQuotesToVersionsAttributes(String yaml) {
        if (StringUtils.isEmpty(yaml)) {
            return yaml;
        }
        return yaml.replace("___VERSIONPROP___", "\"");
    }

    //public static String markWithReplacePattern(String version) {
    //    StringQuotingChecker.Default default_ = new StringQuotingChecker.Default();
    //    version = removeUnfinishedSuffixIfNeeded(version, "\"");
    //    version = removeUnfinishedSuffixIfNeeded(version, "'");
    //    if (StringUtils.isEmpty(version) || version.startsWith("\"") || version.startsWith("'") || version.startsWith("$") || default_
    //            .needToQuoteValue(version)) {
    //        return version;
    //    }
    //    return "___VERSIONPROP___" + version + "___VERSIONPROP___";
    //}
    //
    //private static String removeUnfinishedSuffixIfNeeded(String version, String quoteCharToRemove) {
    //    if (StringUtils.isNotBlank(version) && version.endsWith(quoteCharToRemove) && !version.startsWith(quoteCharToRemove)) {
    //        version = StringUtils.removeEnd(version, quoteCharToRemove);
    //    }
    //    return version;
    //}

    public static String markWithReplacePattern(String version) {
        // 移除未完成的后缀
        version = removeUnfinishedSuffixIfNeeded(version, "\"");
        version = removeUnfinishedSuffixIfNeeded(version, "'");

        // 检查是否需要标记
        if (StringUtils.isEmpty(version) ||
                version.startsWith("\"") ||
                version.startsWith("'") ||
                version.startsWith("$") ||
                needsToBeQuoted(version)) {
            return version;
        }

        // 标记字符串
        return  version;
    }

    private static String removeUnfinishedSuffixIfNeeded(String version, String quoteChar) {
        if (version.endsWith(quoteChar) && !version.endsWith(quoteChar + quoteChar)) {
            return version.substring(0, version.length() - 1);
        }
        return version;
    }

    private static boolean needsToBeQuoted(String value) {
        // 检查是否包含特殊字符，如空格、逗号等
        return value.contains(" ") || value.contains(",") || value.contains(":");
    }
}
