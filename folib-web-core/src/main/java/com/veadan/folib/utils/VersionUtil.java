package com.veadan.folib.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;

/**
 * @author leipenghui
 * @date 2023/9/20
 **/
@Slf4j
public class VersionUtil {

    public static int compareVersions(String version1, String version2) {
        if (version1.equals(version2)) {
            return 0;
        }
        String[] parts1 = version1.split("\\.");
        String[] parts2 = version2.split("\\.");
        int length = Math.max(parts1.length, parts2.length);
        for (int i = 0; i < length; i++) {
            int part1 = (i < parts1.length) ? getVersionComponent(parts1[i]) : 0;
            int part2 = (i < parts2.length) ? getVersionComponent(parts2[i]) : 0;
            if (part1 < part2) {
                return -1;
            } else if (part1 > part2) {
                return 1;
            }
        }
        return 0;
    }

    public static int getVersionComponent(String part) {
        try {
            if (part.contains("-")) {
                part = part.split("-")[0];
            }
            part = part.replaceAll("[^0-9]", "");
            return Integer.parseInt(part);
        } catch (Exception ex) {
            log.error("转换错误 {}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(String.format("Parse [%s] error", part));
        }
    }
}
