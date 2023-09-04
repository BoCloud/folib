package com.veadan.folib.utils;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSONObject;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.apache.commons.lang3.StringUtils;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.function.Function;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/9/5 00:56
 * @since x.x.x
 */
public class CompressUtil 
{
    public static void zip2Targz(String zipPath, String targzPath) throws IOException
    {
        zip2Targz(zipPath, targzPath, null, null, null);
    }
    
    public static void zip2Targz(String zipPath, String targzPath, Function<String, String> zipEntryNameRebuildFunc, Function<String, Boolean> predicate, Function<byte[], byte[]> contentHandler) throws IOException
    {
        FileUtil.touch(targzPath);
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
                    if (null != predicate && predicate.apply(name)) {
                        if (null != contentHandler)
                        { extra = contentHandler.apply(extra); }
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
//        CompressUtil.zip2Targz("/Users/zerowang/Downloads/demo/Specs-master.zip", "/Users/zerowang/Downloads/demo/Specs-master.tar.gz");
////        System.out.println(System.currentTimeMillis() - l + " ms");
////        traverseZip2("/Users/zerowang/Downloads/demo/Specs-master.zip");
////        System.out.println("=========================");
////        traverseZip2("/Users/zerowang/Downloads/demo/Specs-mini.zip");
//
////        System.out.println("=========================");
////
////        traverse("/Users/zerowang/Downloads/demo/Specs-master.tar.gz");
////        traverse("/Users/zerowang/.cocoapods/repos-art/CocoaPods-Proxy/file.tgz", true);
////        System.out.println("=========================");
////        traverse("/Users/zerowang/Downloads/demo/Specs-master.tar.gz", true);
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
//            if (++i >= 20)
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
//                if (++i >= 10)
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
