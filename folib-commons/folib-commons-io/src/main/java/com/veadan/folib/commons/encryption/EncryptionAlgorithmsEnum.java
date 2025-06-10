package com.veadan.folib.commons.encryption;

import java.io.IOException;

/**
 * @author mtodorov
 */
public enum EncryptionAlgorithmsEnum
{

    MD5("MD5", ".md5"),

    SHA1("SHA-1", ".sha1"),

    SHA256("SHA-256", ".sha256"),

    SHA512("SHA-512", ".sha512");


    private String extension;

    private String algorithm;


    EncryptionAlgorithmsEnum(String algorithm, String extension)
    {
        this.algorithm = algorithm;
        this.extension = extension;
    }

    public String getExtension()
    {
        return extension;
    }

    public void setExtension(String extension)
    {
        this.extension = extension;
    }

    public String getAlgorithm()
    {
        return algorithm;
    }

    public void setAlgorithm(String algorithm)
    {
        this.algorithm = algorithm;
    }

    public static EncryptionAlgorithmsEnum fromAlgorithm(String algorithm)
            throws IOException
    {
        if (algorithm.equals(MD5.getAlgorithm()))
        {
            return MD5;
        }
        if (algorithm.equals(SHA1.getAlgorithm()))
        {
            return SHA1;
        }
        if (algorithm.equals(SHA256.getAlgorithm()))
        {
            return SHA256;
        }
        if (algorithm.equals(SHA512.getAlgorithm()))
        {
            return SHA512;
        }

        throw new IOException("Unsupported digest algorithm!");
    }

}
