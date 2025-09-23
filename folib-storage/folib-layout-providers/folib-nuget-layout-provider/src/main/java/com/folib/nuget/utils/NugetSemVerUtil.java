package com.folib.nuget.utils;




public class NugetSemVerUtil {
    public static boolean isSemVerLevel2(String version) {
        if (version == null || version.isEmpty()) {
            return false;
        }

        // 检查基本格式
        String[] parts = version.split("\\.");
        if (parts.length < 3) {
            return false;
        }

        try {
            // 验证主版本号、次版本号和修订号
            String majorStr = parts[0];
            String minorStr = parts[1];
            String patchStr = parts[2].split("-")[0].split("\\+")[0];

            int major = Integer.parseInt(majorStr);
            int minor = Integer.parseInt(minorStr);
            int patch = Integer.parseInt(patchStr);

            if (major < 0 || minor < 0 || patch < 0) {
                return false;
            }

            // 检查 SemVer 2.0.0 特有特性
            if (version.contains("-")) {
                String prerelease = version.substring(version.indexOf("-") + 1);
                if (prerelease.contains("+")) {
                    prerelease = prerelease.substring(0, prerelease.indexOf("+"));
                }

                // SemVer 2.0.0: 预发布标签中包含点号
                if (prerelease.contains(".")) {
                    return true;
                }

                // SemVer 2.0.0: 预发布标签中含有额外连字符
                if (prerelease.indexOf("-", 1) > 0) {
                    return true;
                }
            }

            return false; // 没有SemVer 2.0.0特征，视为SemVer 1.0.0

        } catch (NumberFormatException e) {
            return false;
        }
    }

    public static boolean isPreReleaseVersion(String version) {
        if (version == null || version.isEmpty()) {
            return false;
        }
        // 检查是否包含预发布标签
        return version.contains("-");
    }
}
