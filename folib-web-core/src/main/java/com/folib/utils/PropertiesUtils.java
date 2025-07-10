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
package com.folib.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.jar.JarArchiveInputStream;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;

/**
 * @author veadan
 * @date 2023/4/2
 **/
@Slf4j
public class PropertiesUtils {

    /**
     * 读取properties
     *
     * @param propertiesContext 文件内容
     * @param propertiesKey     key
     * @return key对应的值
     */
    public static String parseProperties(String propertiesContext, String propertiesKey) {
        try {
            Properties properties = new Properties();
            properties.load(new StringReader(propertiesContext));
            return properties.getProperty(propertiesKey);
        } catch (Exception ex) {
            log.error("parseProperties error：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }

    /**
     * 从jar中读取指定文件
     *
     * @param path     path
     * @param fileName fileName
     * @return 指定文件
     */
    public static byte[] getFileFromJar(Path path, String fileName) {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             ArchiveInputStream ais = new JarArchiveInputStream(bis)) {
            ArchiveEntry entry;
            while ((entry = ais.getNextEntry()) != null) {
                if (entry.getName().endsWith(fileName)) {
                    ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                    try (byteArrayOutputStream) {
                        IOUtils.copy(ais, byteArrayOutputStream);
                    } catch (IOException ex) {
                        throw new IOException(ex);
                    }
                    return byteArrayOutputStream.toByteArray();
                }
            }
            return null;
        } catch (IOException e) {
            log.warn("从 {} 中获取 {} 失败：{}", path.toString(), fileName, ExceptionUtils.getStackTrace(e));
            return null;
        }
    }
}
