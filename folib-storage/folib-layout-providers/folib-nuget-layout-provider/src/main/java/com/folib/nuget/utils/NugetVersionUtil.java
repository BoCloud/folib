package com.folib.nuget.utils;

import com.folib.artifact.coordinates.versioning.SemanticVersion;




public class NugetVersionUtil {
    public static boolean lessOrEqual(String version1, String version2) {
        SemanticVersion semVersion1 = SemanticVersion.parse(version1);
        SemanticVersion semVersion2 = SemanticVersion.parse(version2);
        return semVersion1.compareTo(semVersion2) <= 0;
    }

    public static boolean between(String lower, String upper, String version) {
        return lessOrEqual(lower, version) && lessOrEqual(version, upper);
    }
}
