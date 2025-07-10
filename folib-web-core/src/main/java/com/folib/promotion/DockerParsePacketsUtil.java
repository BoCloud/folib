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
package com.folib.promotion;

import com.github.junrar.Archive;
import com.github.junrar.exception.RarException;
import com.github.junrar.rarfile.FileHeader;
import com.folib.utils.CompressionFormatUtils;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.utils.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

public class DockerParsePacketsUtil {

    protected static final Logger logger = LoggerFactory.getLogger(DockerParsePacketsUtil.class);

    public static Path parsePackets(Path filePath, Path destDirectory) {
        try {
            FileInputStream fis = new FileInputStream(filePath.toString());
            // 读取前512个字节，因为TAR头部长度为512字节
            byte[] buffer = new byte[512];
            fis.read(buffer);
            fis.close();
            if (CompressionFormatUtils.isTarFile(buffer)) {
                logger.info("docker upload file is in TAR format.");
                return Paths.get(filePath.toString());
            } else if (CompressionFormatUtils.isGzipFile(buffer)) {
                logger.info("docker upload file is in GZIP format.");
                decompressGzipFile(filePath.toString(), destDirectory.toString());
            } else if (CompressionFormatUtils.isZipFile(buffer)) {
                logger.info("docker upload file is in ZIP format.");
                unzip(filePath.toString(), destDirectory.toString());
            } else if (CompressionFormatUtils.isRarFile(buffer)) {
                logger.info("docker upload file is in RAR format.");
                extractRar(filePath.toString(), destDirectory.toString());
            } else {
                throw new RuntimeException("Docker Upload Unknown file format.");
            }

            String fileNameW = getFileNameWithoutExtension(filePath.getFileName().toString());
            String fileName = String.join(".", fileNameW, "tar");
            String sourceFile = String.join("/", destDirectory.toString(), fileNameW);

            if (isDirectoryExists(sourceFile)) {
                return compressTar(sourceFile, String.join("/", destDirectory.toString(), fileName));
            }
            throw new RuntimeException("Docker Upload Incorrect compression format");
        } catch (IOException e) {
            logger.error("Docker Upload parse Packets file error:{}", e.getMessage());
            e.printStackTrace();
        }
        return null;
    }


    public static boolean isDirectoryExists(String directoryPath) {
        Path path = Paths.get(directoryPath);
        return Files.isDirectory(path) || Files.isRegularFile(path);
    }


    public static String getFileNameWithoutExtension(String filePath) {
        Path path = Paths.get(filePath);
        String fileName = path.getFileName().toString();
        int dotIndex = fileName.lastIndexOf('.');
        if (dotIndex > 0 && dotIndex < fileName.length() - 1) {
            return fileName.substring(0, dotIndex);
        } else {
            return fileName; // No extension found
        }
    }

    public static void unzip(String zipFilePath, String destDirectory) throws IOException {
        File destDir = new File(destDirectory);
        if (!destDir.exists()) {
            destDir.mkdir();
        }
        try (ZipInputStream zipIn = new ZipInputStream(new FileInputStream(zipFilePath))) {
            ZipEntry entry = zipIn.getNextEntry();
            // Iterates over entries in the ZIP file
            while (entry != null) {
                String filePath = destDirectory + File.separator + entry.getName();
                if (!entry.isDirectory()) {
                    // If the entry is a file, extracts it
                    extractFile(zipIn, filePath);
                } else {
                    // If the entry is a directory, make the directory
                    File dir = new File(filePath);
                    dir.mkdirs();
                }
                zipIn.closeEntry();
                entry = zipIn.getNextEntry();
            }
        }
    }

    private static void extractFile(ZipInputStream zipIn, String filePath) throws IOException {
        try (BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(filePath))) {
            byte[] bytesIn = new byte[4096];
            int read = 0;
            while ((read = zipIn.read(bytesIn)) != -1) {
                bos.write(bytesIn, 0, read);
            }
        }
    }

    public static void decompressGzipFile(String gzipFilePath, String destDirectory) throws IOException {

        Path destDirPath = Paths.get(destDirectory);
        if (Files.isDirectory(destDirPath)) {
            String fileNameWithoutExtension = getFileNameWithoutExtension(gzipFilePath);
            Path destFilePath = destDirPath.resolve(fileNameWithoutExtension);
            try (GZIPInputStream gzipIn = new GZIPInputStream(new FileInputStream(gzipFilePath));
                 FileOutputStream fileOut = new FileOutputStream(destFilePath.toFile());
                 BufferedOutputStream bos = new BufferedOutputStream(fileOut)) {
                byte[] buffer = new byte[4096];
                int bytesRead;
                while ((bytesRead = gzipIn.read(buffer)) != -1) {
                    bos.write(buffer, 0, bytesRead);
                }
            }
        } else {
            throw new IOException("The destination path is not a directory: " + destDirectory);
        }
    }


    public static void decompressTarGzFile(String tarGzFilePath, String destDirectory) throws IOException {
        File destDir = new File(destDirectory);
        if (!destDir.exists()) {
            destDir.mkdirs();
        }

        try (FileInputStream fis = new FileInputStream(tarGzFilePath);
             GzipCompressorInputStream gzipIn = new GzipCompressorInputStream(fis);
             TarArchiveInputStream tarIn = new TarArchiveInputStream(gzipIn)) {

            ArchiveEntry entry;
            while ((entry = tarIn.getNextEntry()) != null) {
                if (entry.isDirectory()) {
                    File dir = new File(destDir, entry.getName());
                    if (!dir.exists()) {
                        dir.mkdirs();
                    }
                } else {
                    File outFile = new File(destDir, entry.getName());
                    File parentDir = outFile.getParentFile();
                    if (!parentDir.exists()) {
                        parentDir.mkdirs();
                    }
                    try (BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(outFile))) {
                        byte[] buffer = new byte[4096];
                        int bytesRead;
                        while ((bytesRead = tarIn.read(buffer)) != -1) {
                            bos.write(buffer, 0, bytesRead);
                        }
                    }
                }
            }
        }
    }

    public static Path compressTar(String filePath, String destDirectory) {
        if (Paths.get(filePath).toString().endsWith(".tar")) {
            return Paths.get(filePath);
        }
        File sourceDir = new File(filePath);
        File targetTarFile = new File(destDirectory);

        try (TarArchiveOutputStream tarOutput = new TarArchiveOutputStream(new FileOutputStream(targetTarFile))) {
            // 第二个参数留空，表示从根目录开始打包
            addToArchiveRecursive(tarOutput, sourceDir, "");
            logger.info("Docker Upload  Tar file created successfully.");
            return Paths.get(targetTarFile.getAbsolutePath());
        } catch (IOException e) {
            logger.error("Docker Upload  Error creating tar file: " + e.getMessage());
            throw new RuntimeException("Docker Upload  Error creating tar file: " + e.getMessage());
        }
    }

    private static void addToArchiveRecursive(TarArchiveOutputStream tarOutput, File source, String basePath) throws IOException {
        File[] children = source.listFiles();
        if (children != null) {
            for (File child : children) {
                String childName = basePath + (basePath.isEmpty() ? "" : File.separator) + child.getName();
                TarArchiveEntry entry = new TarArchiveEntry(child, childName);
                tarOutput.putArchiveEntry(entry);

                if (child.isFile()) {
                    try (FileInputStream fis = new FileInputStream(child)) {
                        IOUtils.copy(fis, tarOutput);
                    }
                    tarOutput.closeArchiveEntry();
                } else if (child.isDirectory()) {
                    tarOutput.closeArchiveEntry();
                    addToArchiveRecursive(tarOutput, child, childName);
                }
            }
        }
    }


    public static void extractRar(String rarFilePath, String destDirectory) throws IOException {
        File destDir = new File(destDirectory);
        if (!destDir.exists()) {
            destDir.mkdirs();
        }

        try (Archive archive = new Archive(new File(rarFilePath))) {

            FileHeader fileHeader;
            while ((fileHeader = archive.nextFileHeader()) != null) {
                if (!fileHeader.isDirectory()) {
                    File outFile = new File(destDir, fileHeader.getFileName());
                    File parentDir = outFile.getParentFile();
                    if (!parentDir.exists()) {
                        parentDir.mkdirs();
                    }

                    try (OutputStream os = new FileOutputStream(outFile)) {
                        archive.extractFile(fileHeader, os);
                    } catch (RarException e) {
                        logger.error("docker upload Error extracting RAR file: {}", e.getMessage());
                        throw new RuntimeException(e);
                    }
                } else {
                    File dir = new File(destDir, fileHeader.getFileName());
                    if (!dir.exists()) {
                        dir.mkdirs();
                    }
                }
            }
        } catch (RarException e) {
            logger.error("docker upload Error extracting RAR file: {}", e.getMessage());
            throw new RuntimeException(e);
        }
    }
}
