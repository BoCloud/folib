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
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.apache.commons.lang3.StringUtils;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * @author veadan
 * @date 2023/9/5 00:56
 */
public class CompressUtil 
{
    public static void zip2Targz(String zipPath, String targzPath) throws IOException
    {
        zip2Targz(zipPath, targzPath, null, null, null, null);
    }
    
    public static void zip2Targz(String zipPath, String targzPath, Function<String, Boolean> zipEntryPredicate, Function<String, String> zipEntryNameRebuildFunc, Function<String, Boolean> contentHandlerPredicate, BiFunction<String, byte[], byte[]> contentHandler) throws IOException
    {
        FileUtil.touch(new File(targzPath));
        // 创建tar.gz输出流
        try (final FileOutputStream fos = new FileOutputStream(targzPath);
             final GzipCompressorOutputStream gos = new GzipCompressorOutputStream(fos);
             final TarArchiveOutputStream tos = new TarArchiveOutputStream(gos);

             final FileInputStream fis = new FileInputStream(zipPath);
             final ZipInputStream zis = new ZipInputStream(fis);
             final ByteArrayOutputStream baos = new ByteArrayOutputStream();)
        {
            tos.setLongFileMode(TarArchiveOutputStream.LONGFILE_GNU);

            ZipEntry ze;
            while ((ze = zis.getNextEntry()) != null) {
                String name = ze.getName();
                if (null != zipEntryPredicate && !zipEntryPredicate.apply(name))
                { continue; }
                if (null != zipEntryNameRebuildFunc)
                { name = zipEntryNameRebuildFunc.apply(name); }
                if (StringUtils.isEmpty(name))
                { continue; }

                final TarArchiveEntry tarArchiveEntry = new TarArchiveEntry(name);
                if (ze.isDirectory()) {
                    // 目录处理逻辑
                    tos.putArchiveEntry(tarArchiveEntry);
                } else {
                    // 文件处理逻辑
                    byte[] buffer = new byte[1024];
                    int readLen = -1;
                    baos.reset();
                    while ((readLen = zis.read(buffer)) != -1) {
                        baos.write(buffer, 0, readLen);
                    }

                    byte[] extra = baos.toByteArray();
                    // 按条件进行过滤进行内容重构
                    if (null != contentHandlerPredicate && contentHandlerPredicate.apply(name)) {
                        if (null != contentHandler)
                        { extra = contentHandler.apply(name, extra); }
                    }
                    tarArchiveEntry.setSize(extra.length);
                    tos.putArchiveEntry(tarArchiveEntry);
                    tos.write(extra);
                }
                tos.closeArchiveEntry();
            }
        }
    }

    public static void zipInputSteam2TarGzFile(InputStream zipInputSteam, String targzPath) throws IOException
    {
        zipInputSteam2TarGzFile(zipInputSteam, targzPath, null, null, null, null);
    }

    public static void zipInputSteam2TarGzFile(InputStream zipInputSteam, String targzPath, Function<String, Boolean> zipEntryPredicate, Function<String, String> zipEntryNameRebuildFunc, Function<String, Boolean> contentHandlerPredicate, BiFunction<String, byte[], byte[]> contentHandler) throws IOException
    {
        FileUtil.touch(new File(targzPath));
        // 创建tar.gz输出流
        try (final FileOutputStream fos = new FileOutputStream(targzPath);
             final GzipCompressorOutputStream gos = new GzipCompressorOutputStream(fos);
             final TarArchiveOutputStream tos = new TarArchiveOutputStream(gos);

             final ZipInputStream zis = new ZipInputStream(zipInputSteam);
             final ByteArrayOutputStream baos = new ByteArrayOutputStream();)
        {
            tos.setLongFileMode(TarArchiveOutputStream.LONGFILE_GNU);

            ZipEntry ze;
            while ((ze = zis.getNextEntry()) != null) {
                String name = ze.getName();
                if (null != zipEntryPredicate && !zipEntryPredicate.apply(name))
                { continue; }
                if (null != zipEntryNameRebuildFunc)
                { name = zipEntryNameRebuildFunc.apply(name); }
                if (StringUtils.isEmpty(name))
                { continue; }

                final TarArchiveEntry tarArchiveEntry = new TarArchiveEntry(name);
                if (ze.isDirectory()) {
                    // 目录处理逻辑
                    tos.putArchiveEntry(tarArchiveEntry);
                } else {
                    // 文件处理逻辑
                    byte[] buffer = new byte[1024];
                    int readLen = -1;
                    baos.reset();
                    while ((readLen = zis.read(buffer)) != -1) {
                        baos.write(buffer, 0, readLen);
                    }

                    byte[] extra = baos.toByteArray();
                    // 按条件进行过滤进行内容重构
                    if (null != contentHandlerPredicate && contentHandlerPredicate.apply(name)) {
                        if (null != contentHandler)
                        { extra = contentHandler.apply(name, extra); }
                    }
                    tarArchiveEntry.setSize(extra.length);
                    tos.putArchiveEntry(tarArchiveEntry);
                    tos.write(extra);
                }
                tos.closeArchiveEntry();
            }
        }
    }


//    public static void main(String[] args) throws IOException {
////        final long l = System.currentTimeMillis();
//        final String path = "/Users/zerowang/Downloads/demo/Specs-mini";
//        final String tarPath = path+"111.tar.gz";
////        CompressUtil.zip2Targz(path+".zip",
////                tarPath, 
////                (name) -> {
////                    System.out.println(name);
////                    return  name.matches(".*?/.{1}/.{1}/.{1}/(.*)");
////                },
////                (name -> name.replaceAll(".*?/.{1}/.{1}/.{1}/(.*)", "$1")),
////                (zipEntryName -> zipEntryName.endsWith(".podspec.json")),
////                ((zipEntryName, extra) -> {
////
////                    System.out.println("================================");
////                    System.out.println(zipEntryName);
////                    System.out.println(new String(extra));
////                  return extra;  
////                })
////        );
////        System.out.println(System.currentTimeMillis() - l + " ms");
////        traverseZip2("/Users/zerowang/Downloads/demo/Specs-master.zip");
////        System.out.println("=========================");
////        traverseZip2("/Users/zerowang/Downloads/demo/Specs-mini.zip");
//
////        System.out.println("=========================");
////
////        traverse(tarPath, true);
//        traverse("/Users/zerowang/Downloads/demo/Specs-master111.tar.gz", false);
////        System.out.println("=========================");
////        traverse("/Users/zerowang/.cocoapods/repos-art/CocoaPods-Proxy/file.tgz", true);
//
////        final String s = "Specs-master/";
////        System.out.println(s.substring(s.indexOf("/")+1));
//    }
//
//    public static void traverseZip(String zipFilePath) throws IOException {
//
//        ZipFile zipFile = new ZipFile(zipFilePath);
//
//        Enumeration<? extends ZipEntry> entries = zipFile.getEntries();
//        int i = 0;
//        while(entries.hasMoreElements()){
//            ZipEntry entry = entries.nextElement();
//            String name = entry.getName();
//            System.out.println("zip: " + name);
//            if (++i >= 10)
//            { break; }
//        }
//
//        zipFile.close();
//    }
//
//    public static void traverseZip2(String zipFilePath) throws IOException {
//
//        FileInputStream fis = new FileInputStream(zipFilePath);
//        ZipInputStream zis = new ZipInputStream(fis);
//        ByteArrayOutputStream baos = new ByteArrayOutputStream();
//
//
//        ZipEntry ze;
//        int i = 0;
//        while ((ze = zis.getNextEntry()) != null) {
//
//            String name = ze.getName();
//            long size = ze.getSize();
//
//            System.out.println("zip: " + name);
//
////            if (ze.isDirectory()) {
////                // 目录处理逻辑
////            } else {
////                // 文件处理逻辑
////
////                byte[] buffer = new byte[1024];
////                int readLen = -1;
////                baos.reset();
////                while ((readLen = zis.read(buffer)) != -1) {
////                    baos.write(buffer, 0, readLen);
////                }
////                String content = new String(baos.toByteArray(), StandardCharsets.UTF_8);
////                System.out.println(content);
////                // TODO: 处理文件内容
////                System.out.println("------------------");
////            }
//            if (++i >= 30)
//            { break; }
//        }
//
//        zis.close();
//    }
//
//    public static void traverse(String tarGzPath, boolean printFirst10) throws IOException {
//
//        InputStream is = new FileInputStream(tarGzPath);
//        GzipCompressorInputStream gzIn = new GzipCompressorInputStream(is);
//        TarArchiveInputStream tarIn = new TarArchiveInputStream(gzIn);
//
//        TarArchiveEntry entry;
//        int i = 0;
//        final ArrayList<String> strings = new ArrayList<>();
//        while ((entry = tarIn.getNextTarEntry()) != null) {
//            // 处理每个条目
//            String name = entry.getName();
//            long size = entry.getSize();
//            if (printFirst10)
//            {
//                System.out.println("tarGz: " + name);
//                if (++i >= 30)
//                { break; }
//            }
//            else
//            {
//                if (StringUtils.isNotBlank(name) && name.contains(".podspec.json"))
//                {
//                    try {
//                        // 读取文件内容
//                        byte[] content = new byte[(int) size];
//                        tarIn.read(content, 0, (int) size);
//                        final String str = new String(content);
//                        final JSONObject jsonObject = JSON.parseObject(str);
//                        final JSONObject source = jsonObject.getJSONObject("source");
//                        if (null != source)
//                        {
//                            final String key = source.keySet().stream().sorted().collect(Collectors.joining(","));
//                            if (!strings.contains(key))
//                            {
//                                System.out.println("-------------------------------");
//                                System.out.println();
//                                strings.add(key);
//                                System.out.printf("其他sourceInfo: %s%n", str);
//                            }
//                        }
//                        else
//                        {
//                            System.out.println("-------------------------------");
//                            System.out.println();
//                            System.out.printf("不包含sourceInfoPod：%s%n", str);
//                        }
//                    }catch (Exception e)
//                    {
//                        e.printStackTrace();
//                        System.out.printf("文件读取发送异常文件：%s%n", name);
//                        continue;
//                    }
//                }
//            }
//        }
//
//        tarIn.close();
//        gzIn.close();
//        is.close();
//    }
}
