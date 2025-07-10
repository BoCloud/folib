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


package com.folib.storage.metadata.nuget;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;


import com.folib.artifact.coordinates.versioning.SemanticVersion;
import jakarta.xml.bind.DatatypeConverter;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;

/**
 * Nuget nupkg file representation
 *
 * @author Veadan
 */
public class TempNupkgFile implements AutoCloseable, Nupkg
{
    /**
     * File with package data
     */
    private File file;

    /**
     * Package hash
     */
    private String hash;

    /**
     * Specification file
     */
    private Nuspec nuspec;

    /**
     * Package refresh date
     */
    private Date updated;

    /**
     * Creates a NuGet package from a stream
     *
     * @param input
     *            stream stream with package
     * @throws IOException
     *             data reading error
     * @throws NugetFormatException
     *             the stream does not contain the NuGet package or the format
     *             of the package does not conform to the standard
     */
    public TempNupkgFile(InputStream inputStream)
        throws IOException,
               NugetFormatException
    {
        try
        {
            this.file = File.createTempFile("nupkg", "jnuget");

            this.hash = copyDataAndCalculateHash(inputStream, this.file);

            try (final FileInputStream fileInputStream = new FileInputStream(file))
            {
                this.nuspec = loadNuspec(fileInputStream);
            }
        }
        catch (NoSuchAlgorithmException ex)
        {
            throw new NugetFormatException("Unable to calculate hash of package", ex);
        }
    }

    /**
     * Extract specification file from stream with NuPkg package
     *
     * @param package
     *            stream stream package
     * @return specification file
     * @throws IOException
     *             read error
     * @throws NugetFormatException
     *             XML in the package archive does not conform to the NuGet
     *             specification
     */
    private static final Nuspec loadNuspec(InputStream packageStream)
        throws IOException,
               NugetFormatException
    {
        try (ZipInputStream zipInputStream = new ZipInputStream(packageStream);)
        {
            ZipEntry entry;
            do
            {
                entry = zipInputStream.getNextEntry();
            } while (entry != null && !isNuspecZipEntry(entry));

            if (entry == null)
            {
                return null;
            }

            return Nuspec.parse(zipInputStream);
        }
    }

    /**
     * ZIP attachment is Nuspec XML specification
     *
     * @param entry
     *            zip attachment
     * @return true if the attachment matches the attachment with the
     *         specification
     */
    private static boolean isNuspecZipEntry(ZipEntry entry)
    {
        return !entry.isDirectory() && entry.getName().endsWith(Nuspec.DEFAULT_FILE_EXTENSION);
    }

    /**
     * Creates a temporary file based on the stream.
     *
     * @param inputStream
     *            data stream
     * @param targetFile
     *            file to copy the package to
     * @return data file
     * @throws IOException
     *             read / write error
     * @throws NoSuchAlgorithmException
     *             the system does not have an algorithm for calculating the
     *             value of HASH
     */
    private static String copyDataAndCalculateHash(InputStream inputStream,
                                                   File targetFile)
        throws IOException,
               NoSuchAlgorithmException
    {
        MessageDigest messageDigest = MessageDigest.getInstance(MessageDigestAlgorithms.SHA_512);
        DigestInputStream digestInputStream = new DigestInputStream(inputStream, messageDigest);

        try (FileOutputStream fileOutputStream = new FileOutputStream(targetFile);
                ReadableByteChannel src = Channels.newChannel(digestInputStream);
                FileChannel dest = fileOutputStream.getChannel();)
        {
            fastChannelCopy(src, dest);

            byte[] digest = digestInputStream.getMessageDigest().digest();

            return DatatypeConverter.printBase64Binary(digest);
        }
    }

    /**
     * Copies data from one channel to another
     *
     * @param src
     *            channel source
     * @param dest
     *            destination channel
     * @throws IOException
     *             input / output error
     */
    private static void fastChannelCopy(final ReadableByteChannel src,
                                        final WritableByteChannel dest)
        throws IOException
    {
        final ByteBuffer buffer = ByteBuffer.allocateDirect(16 * 1024);

        while (src.read(buffer) != -1)
        {
            buffer.flip();
            dest.write(buffer);
            buffer.compact();
        }

        buffer.flip();

        while (buffer.hasRemaining())
        {
            dest.write(buffer);
        }
    }

    public String getHash()
    {
        return hash;
    }

    public Nuspec getNuspec()
    {
        return nuspec;
    }

    @Override
    public void close()
    {
        file.delete();
    }

    public InputStream getStream()
        throws IOException
    {
        if (file == null || !file.exists())
        {
            throw new FileNotFoundException("Package file not found");
        }
        else
        {
            return new FileInputStream(file);
        }
    }

    @Override
    public String getFileName()
    {
        return getId() + "." + getVersion().toString() + DEFAULT_EXTENSION;
    }

    @Override
    public Long getSize()
    {
        if (file == null)
        {
            return null;
        }
        return file.length();
    }

    @Override
    public Date getUpdated()
    {
        if (updated == null)
        {
            this.updated = new Date(file.lastModified());
        }
        return updated;
    }

    @Override
    public String getId()
    {
        return getNuspec().getId();
    }

    @Override
    public SemanticVersion getVersion()
    {
        return getNuspec().getVersion();
    }
}
