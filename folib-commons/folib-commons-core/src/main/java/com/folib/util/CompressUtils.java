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

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream;
import org.apache.commons.io.IOUtils;

import java.io.*;
import java.nio.file.Paths;
import java.util.Objects;

/**
 * @author veadan
 **/
@Slf4j
public class CompressUtils {

    /**
     * 解压zip文件到指定文件夹
     *
     * @param zipFileName 源zip文件路径
     * @param destDir     解压后输出路径
     * @throws IOException IO异常，抛出给调用者处理
     */
    public static void unzip(String zipFileName, String destDir) throws IOException {
        try (
                InputStream inputStream = new FileInputStream(zipFileName);
        ) {
            unzip(inputStream, destDir);
        }
    }

    /**
     * 从输入流中获取zip文件，并解压到指定文件夹
     *
     * @param inputStream zip文件输入流，可以是本地文件输入流，也可以是web请求上传流
     * @param destDir     解压后输出路径
     * @throws IOException IO异常，抛出给调用者处理
     */
    public static void unzip(InputStream inputStream, String destDir) throws IOException {
        try (
                BufferedInputStream bufferedInputStream = new BufferedInputStream(inputStream);
                ArchiveInputStream in = new ZipArchiveInputStream(bufferedInputStream);
        ) {
            ArchiveEntry entry = null;
            while (Objects.nonNull(entry = in.getNextEntry())) {
                if (in.canReadEntryData(entry)) {
                    File file = Paths.get(destDir, entry.getName()).toFile();
                    if (!file.getParentFile().exists()) {
                        file.getParentFile().mkdirs();
                    }
                    if (entry.isDirectory()) {
                        if (!file.exists()) {
                            file.mkdirs();
                        }
                    } else {
                        try (
                                OutputStream out = new FileOutputStream(file);
                        ) {
                            IOUtils.copy(in, out);
                        }
                    }
                } else {
                    log.info("NotCanReadEntryData [{}]", entry.getName());
                }
            }
        }
    }

}
