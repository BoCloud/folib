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
import com.folib.commons.http.range.ByteRange;
import com.folib.commons.io.reloading.ReloadableInputStreamHandler;
import com.folib.commons.util.MessageDigestUtils;

import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * This class is based on java.security.DigestInputStream.
 *
 * @author veadan
 */
public class MultipleDigestInputStream
        extends ByteRangeInputStream
{

    private static final String[] DEFAULT_ALGORITHMS = { EncryptionAlgorithmsEnum.MD5.getAlgorithm(),
                                                         EncryptionAlgorithmsEnum.SHA1.getAlgorithm(),
                                                         EncryptionAlgorithmsEnum.SHA256.getAlgorithm(),
                                                         EncryptionAlgorithmsEnum.SHA512.getAlgorithm() };

    private Map<String, MessageDigest> digests = new LinkedHashMap<>();

    private Map<String, String> hexDigests = new LinkedHashMap<>();

    private DigestUpdater digestUpdater = new DigestUpdater();


    public MultipleDigestInputStream(ReloadableInputStreamHandler handler,
                                     ByteRange byteRange)
            throws IOException
    {
        super(handler, byteRange);
    }

    public MultipleDigestInputStream(ReloadableInputStreamHandler handler,
                                     List<ByteRange> byteRanges)
            throws IOException
    {
        super(handler, byteRanges);
    }

    public MultipleDigestInputStream(InputStream is)
            throws NoSuchAlgorithmException
    {
        this(is, DEFAULT_ALGORITHMS);
    }

    public MultipleDigestInputStream(InputStream is,
                                     String[] algorithms)
            throws NoSuchAlgorithmException
    {
        super(is);

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

    public void setDigests(Map<String, MessageDigest> digests)
    {
        this.digests = digests;
    }

    public Map<String, String> getHexDigests()
    {
        return hexDigests;
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

    @Override
    public int read()
            throws IOException
    {
        if (hasReachedLimit())
        {
            return -1;
        }

        int ch = in.read();
        if (ch != -1)
        {
            for (Map.Entry entry : digests.entrySet())
            {
                MessageDigest digest = (MessageDigest) entry.getValue();
                digest.update((byte) ch);
            }
        }

        bytesRead++;

        return ch;
    }

    @Override
    public int read(byte[] bytes,
                    int off,
                    int len)
            throws IOException
    {
        if (hasReachedLimit())
        {
            return -1;
        }

        int numberOfBytesRead = in.read(bytes, off, len);
        if (numberOfBytesRead != -1)
        {
//            DigestUpdater.updateDigestsConcurrently(digests, bytes, off, numberOfBytesRead);
            digestUpdater.updateDigestsConcurrently(digests, bytes, off, numberOfBytesRead);
        }

        if (limit > 0 && bytesRead < limit)
        {
            bytesRead += numberOfBytesRead;
        }

        return numberOfBytesRead;
    }

    @Override
    public int read(byte[] bytes)
            throws IOException
    {
        if (hasReachedLimit())
        {
            return -1;
        }

        int len = in.read(bytes);

//        DigestUpdater.updateDigestsConcurrently(digests, bytes);
        digestUpdater.updateDigestsConcurrently(digests, bytes);

        bytesRead += len;

        if (limit > 0 && bytesRead < limit)
        {
            bytesRead += len;
        }

        return len;
    }

    @Override
    public void reload()
            throws IOException
    {
        reloadableInputStreamHandler.reload();
        in = reloadableInputStreamHandler.getInputStream();
    }

    @Override
    public void reposition()
            throws IOException
    {
        if (byteRanges != null && !byteRanges.isEmpty() && currentByteRangeIndex < byteRanges.size())
        {
            ByteRange current = currentByteRange;

            currentByteRangeIndex++;
            currentByteRange = byteRanges.get(currentByteRangeIndex);

            if (currentByteRange.getOffset() > current.getLimit())
            {
                // If the offset is higher than the current position, skip forward
                long bytesToSkip = currentByteRange.getOffset() - current.getLimit();

                //noinspection ResultOfMethodCallIgnored
                in.skip(bytesToSkip);
            }
            else
            {
                reloadableInputStreamHandler.reload();
                in = reloadableInputStreamHandler.getInputStream();
            }
        }
    }

    @Override
    public void reposition(long skipBytes)
    {

    }

    @Override
    public void close()
            throws IOException
    {
        super.close();

        digestUpdater.clearThreadPool();
    }



}
