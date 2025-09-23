package com.folib.index.utils;

public class CondaPathUtils {
    public static String getFileName(String path) {
        if (path == null || path.isEmpty()) {
            return null;
        }
        String[] parts = path.split("/");
        return parts[parts.length - 1];
    }

    public static CharSequence trimSlashes(String path) {
        if (path == null || path.isEmpty()) {
            return null;
        }
        String trimmedPath = path.replaceAll("^/+", "").replaceAll("/+$", "");
        return trimmedPath;
    }

    public static String getExtension(String filePath) {
        if (filePath == null || filePath.isEmpty()) {
            return null;
        }
        int lastDotIndex = filePath.lastIndexOf(".");
        if (lastDotIndex == -1) {
            return "";
        }
        return filePath.substring(lastDotIndex + 1);
    }
}
