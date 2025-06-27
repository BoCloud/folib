package com.veadan.folib.commons.util;

import java.io.*;
import java.security.MessageDigest;

/**
 * @author veadan
 */
public class MessageDigestUtils
{

    private MessageDigestUtils()
    {
    }

    public static String convertToHexadecimalString(MessageDigest md)
    {
        byte[] hash = md.digest();
        StringBuilder sb = new StringBuilder(2 * hash.length);
        for (byte b : hash)
        {
            sb.append(String.format("%02x", b & 0xff));
        }

        return sb.toString();
    }

    public static String readChecksumFile(String path)
            throws IOException
    {
        try (InputStream is = new FileInputStream(path))
        {
            return readChecksumFile(is);
        }
    }

    private static String readChecksumFile(InputStream is)
            throws IOException
    {
        try (BufferedReader br = new BufferedReader(new InputStreamReader(is)))
        {
            return br.readLine();
        }
    }

}
