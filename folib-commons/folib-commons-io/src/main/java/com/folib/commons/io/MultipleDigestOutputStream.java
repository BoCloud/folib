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
package com.folib.commons.io;

import com.folib.commons.encryption.EncryptionAlgorithmsEnum;
import com.folib.commons.util.MessageDigestUtils;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author veadan
 */
public class MultipleDigestOutputStream extends FilterOutputStream
{

    private static final String[] DEFAULT_ALGORITHMS = { EncryptionAlgorithmsEnum.MD5.getAlgorithm(),
                                                         EncryptionAlgorithmsEnum.SHA1.getAlgorithm(),
                                                         EncryptionAlgorithmsEnum.SHA256.getAlgorithm(),
                                                         EncryptionAlgorithmsEnum.SHA512.getAlgorithm() };

    private Map<String, MessageDigest> digests = new LinkedHashMap<>();

    private Map<String, String> hexDigests = new LinkedHashMap<>();

    private Path path;

    private boolean generateChecksumFiles;

    private DigestUpdater digestUpdater = new DigestUpdater();


    public MultipleDigestOutputStream(OutputStream os)
            throws NoSuchAlgorithmException
    {
        super(os);

        addAlgorithms(DEFAULT_ALGORITHMS);
    }

    public MultipleDigestOutputStream(OutputStream os, String[] algorithms)
            throws NoSuchAlgorithmException
    {
        super(os);

        addAlgorithms(algorithms);
    }

    public MultipleDigestOutputStream(File file, OutputStream os)
            throws NoSuchAlgorithmException
    {
        this(Paths.get(file.getAbsolutePath()), os, DEFAULT_ALGORITHMS, true);
    }

    public MultipleDigestOutputStream(Path path, OutputStream os)
            throws NoSuchAlgorithmException
    {
        this(path, os, DEFAULT_ALGORITHMS, true);
    }

    public MultipleDigestOutputStream(Path path,
                                      OutputStream os,
                                      String[] algorithms,
                                      boolean generateChecksumFiles)
            throws NoSuchAlgorithmException
    {
        super(os);

        this.path = path;
        this.generateChecksumFiles = generateChecksumFiles;

        addAlgorithms(algorithms);
    }

    private void addAlgorithms(String[] algorithms)
            throws NoSuchAlgorithmException
    {
        for (String algorithm : algorithms)
        {
            addAlgorithm(algorithm);
        }
    }

    public void addAlgorithm(String algorithm)
            throws NoSuchAlgorithmException
    {
        MessageDigest digest = MessageDigest.getInstance(algorithm);

        digests.put(algorithm, digest);
    }

    public MessageDigest getMessageDigest(String algorithm)
    {
        return digests.get(algorithm);
    }

    public Map<String, MessageDigest> getDigests()
    {
        return digests;
    }

    public String getMessageDigestAsHexadecimalString(String algorithm)
    {
        if (hexDigests.containsKey(algorithm))
        {
            return hexDigests.get(algorithm);
        }
        else
        {
            // This method will invoke MessageDigest.digest() which will reset the bytes when it's done
            // and thus this data will no longer be available, so we'll need to cache the calculated digest
            String hexDigest = MessageDigestUtils.convertToHexadecimalString(getMessageDigest(algorithm));
            hexDigests.put(algorithm, hexDigest);
            return hexDigest;
        }
    }

    public void setDigests(Map<String, MessageDigest> digests)
    {
        this.digests = digests;
    }

    @Override
    public void write(int b)
            throws IOException
    {
        out.write(b);

    }

    @Override
    public void write(byte[] b)
            throws IOException
    {
        out.write(b);

//        DigestUpdater.updateDigestsConcurrently(digests, b);
        digestUpdater.updateDigestsConcurrently(digests, b);
    }

    @Override
    public void write(byte[] b, int off, int len)
            throws IOException
    {
        out.write(b, off, len);

//        DigestUpdater.updateDigestsConcurrently(digests, b, off, len);
        digestUpdater.updateDigestsConcurrently(digests, b, off, len);
    }

    @Override
    public void close()
            throws IOException
    {
        super.close();

        if (generateChecksumFiles)
        {
            writeChecksums();
        }

        digestUpdater.clearThreadPool();
    }

    public void writeChecksums()
            throws IOException
    {
        for (Map.Entry entry : digests.entrySet())
        {
            MessageDigest digest = (MessageDigest) entry.getValue();

            String hexDigest = getMessageDigestAsHexadecimalString(digest.getAlgorithm());

            writeChecksum(path, digest.getAlgorithm(), hexDigest);
        }
    }

    private void writeChecksum(Path path, String algorithm, String hexDigest)
            throws IOException
    {
        final String filePathStr =
                path.toAbsolutePath().toString() + EncryptionAlgorithmsEnum.fromAlgorithm(algorithm).getExtension();

        try (FileWriter fw = new FileWriter(filePathStr))
        {
            fw.write(hexDigest + "\n");
            fw.flush();
        }
    }

    public Path getPath()
    {
        return path;
    }

    public void setPath(Path path)
    {
        this.path = path;
    }

    public boolean isGenerateChecksumFiles()
    {
        return generateChecksumFiles;
    }

    public void setGenerateChecksumFiles(boolean generateChecksumFiles)
    {
        this.generateChecksumFiles = generateChecksumFiles;
    }

}
