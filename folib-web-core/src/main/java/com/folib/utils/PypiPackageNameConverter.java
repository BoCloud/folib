package com.folib.utils;

/**
 * Utility class for various operations on python package name. 
 * 
 * @author veadan
 */
public class PypiPackageNameConverter
{

    public static String escapeSpecialCharacters(String packageName)
    {
        // https://www.python.org/dev/peps/pep-0427/#escaping-and-unicode
        return packageName.replaceAll("[^A-Za-z0-9 ]", "_");
    }
}
