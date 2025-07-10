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

import cn.hutool.core.io.FileUtil;
import lombok.Data;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.apache.commons.lang3.StringUtils;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author veadan
 * @date 2023/8/2 11:23
 */
public class CocoapodsArtifactUtil {
    private static final Pattern PODSPEC_HEAD_LINE_PATTERN = Pattern.compile("Pod\\:\\:Spec\\.new\\s+?do\\s+?\\|(.*?)\\|");

    /**
     * 替换Pod.tar.gz文件输入流中的*.podspec文件*.source属性，并另存为Pod.tar.gz文件
     *
     * @param inputStream      Pod.tar.gz文件输入流
     * @param newSourceUrl     新的SourceUrl（非 Pod.tar.gz Url，例如：http://10.10.33.149:8081/artifactory/api/pods/Cocoapad-Local/pod/pkg/AFNetworking/4.0.1 ）
     * @param newTarGzFilePath 新的Pod.tar.gz存储路径
     * @return 结果
     */
    public static boolean replacePodspecSourceSaveAsNewTarGzFile(InputStream inputStream, String newSourceUrl, String newTarGzFilePath) {
        try (InputStream gzipInputStream = new GzipCompressorInputStream(inputStream);
             TarArchiveInputStream tarInputStream = new TarArchiveInputStream(gzipInputStream);
             OutputStream outputStream = Files.newOutputStream(Paths.get(newTarGzFilePath));
             OutputStream gzipOutputStream = new GzipCompressorOutputStream(outputStream);
             TarArchiveOutputStream tarOutputStream = new TarArchiveOutputStream(gzipOutputStream)) {

            // 支持长文件名
            tarOutputStream.setLongFileMode(TarArchiveOutputStream.LONGFILE_GNU);

            TarArchiveEntry entry;
            while ((entry = tarInputStream.getNextTarEntry()) != null) {
                if (!entry.isDirectory()) {
                    // 处理非目录文件
                    String entryName = entry.getName();
                    byte[] content = new byte[(int) entry.getSize()];
                    tarInputStream.read(content);

                    if (entryName.endsWith(".podspec")) {
                        final String podspecContent = new String(content);
                        final Matcher matcher = PODSPEC_HEAD_LINE_PATTERN.matcher(podspecContent);
                        if (matcher.find()) {
                            final String headVar = matcher.group(1);
                            final String newSourceInfo = String.format("{ :http => \"%s\", :type => 'tgz'}", newSourceUrl);
                            final String newPodspecContent = podspecContent.replaceAll("(" + headVar + "\\.source\\s+?=\\s+?).*", String.format("$1%s", newSourceInfo));
                            content = newPodspecContent.getBytes(StandardCharsets.UTF_8);
                        } else {
                            throw new RuntimeException("未找到podspec文件头变量名称");
                        }
                    }

                    // 将编辑后的内容写回tar文件中
                    TarArchiveEntry updatedEntry = new TarArchiveEntry(entryName);
                    updatedEntry.setSize(content.length);
                    tarOutputStream.putArchiveEntry(updatedEntry);
                    tarOutputStream.write(content);
                    tarOutputStream.closeArchiveEntry();
                } else {
                    // 处理目录
                    tarOutputStream.putArchiveEntry(entry);
                    tarOutputStream.closeArchiveEntry();
                }
            }

            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * 获取Pod.tar.gz压缩包中的.podspec文件内容并替换*.podspec文件中*.source属性为新的newSourceUrl
     *
     * @param inputStream  Pod.tar.gz文件输入流
     * @param newSourceUrl 新的SourceUrl（非 Pod.tar.gz Url，例如：http://10.10.33.149:8081/artifactory/api/pods/Cocoapad-Local/pod/pkg/AFNetworking/4.0.1 ）
     * @return 新的*.podspec文件内容
     */
    public static String fetchReplacePodspecSourceContent(InputStream inputStream, String newSourceUrl) {
        try (InputStream gzipInputStream = new GzipCompressorInputStream(inputStream);
             TarArchiveInputStream tarInputStream = new TarArchiveInputStream(gzipInputStream);) {

            TarArchiveEntry entry;
            while ((entry = tarInputStream.getNextTarEntry()) != null) {
                if (!entry.isDirectory()) {
                    // 处理非目录文件
                    String entryName = entry.getName();
                    byte[] content = new byte[(int) entry.getSize()];
                    tarInputStream.read(content);

                    if (entryName.endsWith(".podspec")) {
                        final String podspecContent = new String(content);
                        final String newPodspecContent = replaceNewSourceUrlOfPodspecContent(podspecContent, newSourceUrl);
                        if (null != newPodspecContent) {
                            return newPodspecContent;
                        } else {
                            throw new RuntimeException("未找到podspec文件头变量名称");
                        }
                    }
                }
            }

            return null;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * 替换Podspec内容中的SourceUrl为newSourceUrl
     *
     * @param podspecContent
     * @param newSourceUrl
     * @return
     */
    public static String replaceNewSourceUrlOfPodspecContent(String podspecContent, String newSourceUrl) {
        final Matcher matcher = PODSPEC_HEAD_LINE_PATTERN.matcher(podspecContent);
        if (matcher.find()) {
            final String headVar = matcher.group(1);
            final String newSourceInfo = String.format("{ :http => \"%s\", :type => 'tgz'}", newSourceUrl);
            return podspecContent.replaceAll("(" + headVar + "\\.source\\s+?=\\s+?).*", String.format("$1%s", newSourceInfo));
        }

        return null;
    }

    public static String fetchPodspecSourceContentByTarGzFile(String tarGzFilePath) {
        if (FileUtil.exist(tarGzFilePath)) {
            final BufferedInputStream inputStream = FileUtil.getInputStream(tarGzFilePath);
            return fetchPodspecSourceContentByInputStream(inputStream);
        }

        return null;
    }

    public static String fetchPodspecSourceContentByInputStream(InputStream inputStream) {
        if (null == inputStream) {
            return null;
        }

        try (BufferedInputStream bis = new BufferedInputStream(inputStream);
             InputStream gzipInputStream = new GzipCompressorInputStream(bis);
             TarArchiveInputStream tarInputStream = new TarArchiveInputStream(gzipInputStream);) {

            TarArchiveEntry entry;
            while ((entry = tarInputStream.getNextTarEntry()) != null) {
                if (!entry.isDirectory()) {
                    // 处理非目录文件
                    String entryName = entry.getName();
                    byte[] content = new byte[(int) entry.getSize()];
                    tarInputStream.read(content);

                    if (entryName.endsWith(".podspec")) {
                        return new String(content);
                    }
                }
            }

            return null;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
    
    public static PodSpec resolvePodSpec(String podSpecContent) {
        if (StringUtils.isNotBlank(podSpecContent)) {
            final PodSpec podSpec = new PodSpec();
            podSpec.setName(findAttr(podSpecContent, "name"));
            podSpec.setVersion(findAttr(podSpecContent, "version"));
            podSpec.setLicense(findAttr(podSpecContent, "license"));
            return podSpec;
        }

        return null;
    }

    public static PodSpec resolvePodSpecByTarGzFile(String podTarGzFilePath) {
        final String podSpecContent = fetchPodspecSourceContentByTarGzFile(podTarGzFilePath);
        if (StringUtils.isNotBlank(podSpecContent)) {
            final PodSpec podSpec = new PodSpec();
            podSpec.setName(findAttr(podSpecContent, "name"));
            podSpec.setVersion(findAttr(podSpecContent, "version"));
            podSpec.setLicense(findAttr(podSpecContent, "license"));
            return podSpec;
        }

        return null;
    }

    private static String podspecHeadName(String podSpecContent) {
        final Matcher matcher = PODSPEC_HEAD_LINE_PATTERN.matcher(podSpecContent);
        if (matcher.find()) {
            return matcher.group(1);
        }

        return null;
    }

    /**
     * 查找属性（TODO：2023/8/3 16:48 此方法只支持查找 `s.name     = 'AFNetworking'` 简单属性，不支持查找复杂结构）
     *
     * @param podSpecContent
     * @param attr
     * @return
     */
    private static String findAttr(String podSpecContent, String attr) {
        final String podspecHeadName = podspecHeadName(podSpecContent);
        final Pattern pattern = Pattern.compile(String.format("%s\\.%s\\s+?=\\s+?['|\"](.*?)['|\"]", podspecHeadName, attr));
        final Matcher matcher = pattern.matcher(podSpecContent);
        if (matcher.find()) {
            return matcher.group(1);
        }

        return null;
    }

    /**
     * @author veadan
     * @date 2023/8/3 16:14
     */
    @Data
    public static class PodSpec {
        private String name;
        private String version;
        private String license;
    }
}
