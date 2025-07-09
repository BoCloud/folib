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
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.io.IOUtils;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

/**
 * @author veadan
 */
//@FunctionalInterface
public interface ArchiveListingFunction {

    Set<String> listFilenames(RepositoryPath path)
            throws IOException;

    byte[] getContentByFileName(RepositoryPath path, String fileName) throws IOException;

    byte[] getContentByFileName(RepositoryPath repositoryPath, Path path, String fileName) throws IOException;

    byte[] getContentByEqualsFileName(RepositoryPath repositoryPath, Path path, String fileName) throws IOException;

    default byte[] getContentByFileName(final ArchiveInputStream archiveInputStream, String fileName) throws IOException {
        ArchiveEntry entry;
        while ((entry = archiveInputStream.getNextEntry()) != null) {
            if (entry.getName().endsWith(fileName)) {
                ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                try (byteArrayOutputStream) {
                    IOUtils.copy(archiveInputStream, byteArrayOutputStream);
                } catch (IOException ex) {
                    throw new IOException(ex);
                }
                return byteArrayOutputStream.toByteArray();
            }
        }
        return null;
    }

    default byte[] getContentByEqualsFileName(final ArchiveInputStream archiveInputStream, String fileName) throws IOException {
        ArchiveEntry entry;
        while ((entry = archiveInputStream.getNextEntry()) != null) {
            if (entry.getName().equals(fileName)) {
                ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                try (byteArrayOutputStream) {
                    IOUtils.copy(archiveInputStream, byteArrayOutputStream);
                } catch (IOException ex) {
                    throw new IOException(ex);
                }
                return byteArrayOutputStream.toByteArray();
            }
        }
        return null;
    }

    default Set<String> getEntryNames(final ArchiveInputStream archiveInputStream)
            throws IOException {
        final Set<String> result = new HashSet<>();
        ArchiveEntry entry;
        while ((entry = archiveInputStream.getNextEntry()) != null) {
            result.add(entry.getName());
        }
        return result;
    }

    default boolean supports(RepositoryPath path) {
        return true;
    }
}
