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
package com.folib.io;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.folib.enums.ProductTypeEnum;
import com.folib.util.MessageDigestUtils;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.io.input.ProxyInputStream;

/**
 * This class decorates storage {@link InputStream} with common layout specific logic.
 *
 * You don't need to instantiate it directly, see example below:
 *
 * <pre>
 *     RepositoryPath repositoryPath = repositlryPathResolver.resolve("path/to/your/artifact/file.ext");
 *     ArtifactInputStream aos = (ArtifactInputStream) Files.newInputStream(repositoryPath);
 * </pre>
 *
 * @author veadan
 *
 */


public class LayoutInputStream
        extends ProxyInputStream
{

    private static final Set<String> DEFAULT_ALGORITHM_SET = Stream.of(MessageDigestAlgorithms.MD5,
            MessageDigestAlgorithms.SHA_1)
            .collect(Collectors.toSet());

    private Map<String, MessageDigest> digests = new LinkedHashMap<>();

    private Map<String, String> hexDigests = new LinkedHashMap<>();

    public LayoutInputStream(InputStream is,
                             Set<String> checkSumDigestAlgorithmSet)
            throws NoSuchAlgorithmException
    {
        super(new BufferedInputStream(is));

        for (String algorithm : checkSumDigestAlgorithmSet)
        {
            addAlgorithm(algorithm);
        }
    }

    public LayoutInputStream(InputStream is)
            throws NoSuchAlgorithmException
    {
        this(is, DEFAULT_ALGORITHM_SET);
    }

    public final void addAlgorithm(String algorithm)
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

    public void resetHexDidests()
    {
        hexDigests.clear();
    }

    public Map<String, String> getHexDigests()
    {
        return hexDigests;
    }

    public String getMessageDigestAsHexadecimalString(String algorithm, String layout)
    {
        if (hexDigests.containsKey(algorithm))
        {
            return hexDigests.get(algorithm);
        }
        else
        {
            // This method will invoke MessageDigest.digest() which will reset the bytes when it's done
            // and thus this data will no longer be available, so we'll need to cache the calculated digest
            String hexDigest;
            if (ProductTypeEnum.Npm.getFoLibraryName().equalsIgnoreCase(layout) && MessageDigestAlgorithms.SHA_512.equals(algorithm)) {
                hexDigest = printBase64Binary(getMessageDigest(algorithm).digest());
            } else {
                hexDigest = MessageDigestUtils.convertToHexadecimalString(getMessageDigest(algorithm));
            }
            hexDigests.put(algorithm, hexDigest);

            return hexDigest;
        }
    }

    public void setDigests(Map<String, MessageDigest> digests)
    {
        this.digests = digests;
    }

    public Map<String, String> getDigestMap()
    {
        return hexDigests;
    }

    @Override
    public int read()
            throws IOException
    {
        int ch = in.read();
        if (ch != -1)
        {
            for (Map.Entry entry : digests.entrySet())
            {
                MessageDigest digest = (MessageDigest) entry.getValue();
                digest.update((byte) ch);
            }
        }

        return ch;
    }

    @Override
    public int read(byte[] bytes,
                    int off,
                    int len)
            throws IOException
    {
        int numberOfBytesRead = in.read(bytes, off, len);
        if (numberOfBytesRead != -1)
        {
            for (Map.Entry entry : digests.entrySet())
            {
                MessageDigest digest = (MessageDigest) entry.getValue();
                digest.update(bytes, off, numberOfBytesRead);
            }
        }
        return numberOfBytesRead;
    }

    @Override
    public int read(byte[] bytes)
            throws IOException
    {
        int len = in.read(bytes);
        for (Map.Entry entry : digests.entrySet())
        {
            MessageDigest digest = (MessageDigest) entry.getValue();
            if (len != -1) {
                digest.update(bytes, 0, len);
            }
        }
        return len;
    }

    InputStream getTarget()
    {
        return in;
    }

    protected String printBase64Binary(
            byte[] digest)
    {
        return String.format("%s-%s", "sha512", Base64.getEncoder().encodeToString(digest));
    }
}
