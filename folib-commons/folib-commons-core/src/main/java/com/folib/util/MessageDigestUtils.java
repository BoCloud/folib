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
package com.folib.util;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class MessageDigestUtils
{

    private static final Logger logger = LoggerFactory.getLogger(MessageDigestUtils.class);

    private MessageDigestUtils() 
    {
    }


    public static String convertToHexadecimalString(MessageDigest md)
    {
        byte[] hash = md.digest();
        return convertToHexadecimalString(hash);
    }


    public static String convertToHexadecimalString(byte[] hash)
    {
        StringBuilder sb = new StringBuilder(2 * hash.length);
        for (byte b : hash)
        {
            sb.append(String.format("%02x", b & 0xff));
        }

        return sb.toString();
    }

    public static void writeDigestAsHexadecimalString(MessageDigest digest,
                                                      Path artifactPath,
                                                      String checksumFileExtension)
            throws IOException
    {
        String checksum = MessageDigestUtils.convertToHexadecimalString(digest);

        writeChecksum(artifactPath, checksumFileExtension, checksum);
    }

    public static void writeChecksum(Path artifactPath, String checksumFileExtension, String checksum)
            throws IOException
    {
        final Path checksumPath = artifactPath.resolveSibling(artifactPath.getFileName() + checksumFileExtension);
        try (OutputStream fos = Files.newOutputStream(checksumPath))
        {
            fos.write((checksum).getBytes());
            fos.flush();
        }
    }

    public static String readChecksumFile(String path)
            throws IOException
    {
        try (InputStream is = new FileInputStream(path))
        {
            return readChecksumFile(is);
        }
    }

    public static String readChecksumFile(InputStream is)
            throws IOException
    {
        try (BufferedReader br = new BufferedReader(new InputStreamReader(is)))
        {
            return br.readLine();
        }
    }

    public static String calculateChecksum(Path path,
                                           String type)
            throws IOException, NoSuchAlgorithmException
    {
        byte[] buffer = new byte[4096];
        MessageDigest md = MessageDigest.getInstance(type);

        try (DigestInputStream dis = new DigestInputStream(Files.newInputStream(path), md))
        {
            while (dis.read(buffer) != -1) ;
        }

        return convertToHexadecimalString(md);
    }
}
