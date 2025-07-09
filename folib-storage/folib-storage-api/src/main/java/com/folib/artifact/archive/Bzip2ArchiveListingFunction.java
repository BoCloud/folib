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
package com.folib.artifact.archive;

import com.folib.providers.io.RepositoryPath;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

/**
 * @author veadan
 */
public enum Bzip2ArchiveListingFunction
        implements ArchiveListingFunction {

    /**
     * 实例
     */
    INSTANCE;

    @Override
    public Set<String> listFilenames(final RepositoryPath path)
            throws IOException {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             BZip2CompressorInputStream bzIs = new BZip2CompressorInputStream(bis);
             ArchiveInputStream tarIs = new TarArchiveInputStream(bzIs)) {
            return getEntryNames(tarIs);
        }
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath path, String fileName) throws IOException {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             BZip2CompressorInputStream bzIs = new BZip2CompressorInputStream(bis);
             ArchiveInputStream tarIs = new TarArchiveInputStream(bzIs)) {
            return getContentByFileName(tarIs, fileName);
        }
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath repositoryPath, Path path, String fileName) throws IOException {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             BZip2CompressorInputStream bzIs = new BZip2CompressorInputStream(bis);
             ArchiveInputStream tarIs = new TarArchiveInputStream(bzIs)) {
            return getContentByFileName(tarIs, fileName);
        }
    }

    @Override
    public byte[] getContentByEqualsFileName(RepositoryPath repositoryPath, Path path, String fileName) throws IOException {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             BZip2CompressorInputStream bzIs = new BZip2CompressorInputStream(bis);
             ArchiveInputStream tarIs = new TarArchiveInputStream(bzIs)) {
            return getContentByEqualsFileName(tarIs, fileName);
        }
    }

    @Override
    public boolean supports(final RepositoryPath path) {
        final Path fileName = path.getFileName();
        return fileName != null && fileName.toString().endsWith("tar.bz2");
    }
}
