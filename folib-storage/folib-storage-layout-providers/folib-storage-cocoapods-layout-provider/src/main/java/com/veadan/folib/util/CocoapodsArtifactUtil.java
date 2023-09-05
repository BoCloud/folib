package com.veadan.folib.util;

import cn.hutool.core.io.FileUtil;
import lombok.Data;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/2 11:23
 * @since x.x.x
 */
public class CocoapodsArtifactUtil 
{
    private static final Pattern PODSPEC_HEAD_LINE_PATTERN = Pattern.compile("Pod\\:\\:Spec\\.new\\s+?do\\s+?\\|(.*?)\\|");
    
    /**
     * 替换Pod.tar.gz文件输入流中的*.podspec文件*.source属性，并另存为Pod.tar.gz文件
     * @param inputStream Pod.tar.gz文件输入流
     * @param newSourceUrl 新的SourceUrl（非 Pod.tar.gz Url，例如：http://10.10.33.149:8081/artifactory/api/pods/Cocoapad-Local/pod/pkg/AFNetworking/4.0.1 ）
     * @param newTarGzFilePath 新的Pod.tar.gz存储路径
     * @return 结果
     * @since x.x.x
     */
    public static boolean replacePodspecSourceSaveAsNewTarGzFile(InputStream inputStream, String newSourceUrl, String newTarGzFilePath)
    {
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
                        }
                        else
                        { throw new RuntimeException("未找到podspec文件头变量名称"); }
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
        }
        catch (Exception e)
        { 
            e.printStackTrace();
            return false; 
        }
    }

    /**
     * 获取Pod.tar.gz压缩包中的.podspec文件内容并替换*.podspec文件中*.source属性为新的newSourceUrl
     * @param inputStream Pod.tar.gz文件输入流
     * @param newSourceUrl 新的SourceUrl（非 Pod.tar.gz Url，例如：http://10.10.33.149:8081/artifactory/api/pods/Cocoapad-Local/pod/pkg/AFNetworking/4.0.1 ） 
     * @return 新的*.podspec文件内容
     * @since x.x.x
     */
    public static String fetchReplacePodspecSourceContent(InputStream inputStream, String newSourceUrl)
    {
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
                        if (null != newPodspecContent) 
                        { return newPodspecContent; }
                        else
                        { throw new RuntimeException("未找到podspec文件头变量名称"); }
                    }
                }
            }

            return null;
        }
        catch (Exception e)
        {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * 替换Podspec内容中的SourceUrl为newSourceUrl
     * @param podspecContent
     * @param newSourceUrl
     * @return
     * @since x.x.x
     */
    public static String replaceNewSourceUrlOfPodspecContent(String podspecContent, String newSourceUrl)
    {
        final Matcher matcher = PODSPEC_HEAD_LINE_PATTERN.matcher(podspecContent);
        if (matcher.find()) {
            final String headVar = matcher.group(1);
            final String newSourceInfo = String.format("{ :http => \"%s\", :type => 'tgz'}", newSourceUrl);
            return podspecContent.replaceAll("(" + headVar + "\\.source\\s+?=\\s+?).*", String.format("$1%s", newSourceInfo));
        }
        
        return null;
    }

    public static String fetchPodspecSourceContent(InputStream inputStream)
    {
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
                        return new String(content);
                    }
                }
            }

            return null;
        }
        catch (Exception e)
        {
            e.printStackTrace();
            return null;
        }
    }
    
    public static PodSpec resolvePodSpec(String podSpecContent)
    {
        final PodSpec podSpec = new PodSpec();
        podSpec.setName(findAttr(podSpecContent, "name"));
        podSpec.setVersion(findAttr(podSpecContent, "version"));
        podSpec.setLicense(findAttr(podSpecContent, "license"));
        return podSpec;
    }
    
    private static String podspecHeadName(String podSpecContent)
    {
        final Matcher matcher = PODSPEC_HEAD_LINE_PATTERN.matcher(podSpecContent);
        if (matcher.find())
        { return matcher.group(1); }
        
        return null;
    }

    /**
     * 查找属性（TODO：2023/8/3 16:48 此方法只支持查找 `s.name     = 'AFNetworking'` 简单属性，不支持查找复杂结构）
     * @param podSpecContent
     * @param attr
     * @return
     * @since x.x.x
     */
    private static String findAttr(String podSpecContent, String attr)
    {
        final String podspecHeadName = podspecHeadName(podSpecContent);
        final Pattern pattern = Pattern.compile(String.format("%s\\.%s\\s+?=\\s+?['|\"](.*?)['|\"]", podspecHeadName, attr));
        final Matcher matcher = pattern.matcher(podSpecContent);
        if (matcher.find())
        { return matcher.group(1); }

        return null;
    }
    
    public static void main(String[] args) throws IOException {
        final String path = "/Users/zerowang/BoCloudWork/02_CodeManage/000_BoCloud/folib-server/folib-storage/folib-storage-layout-providers/folib-storage-cocoapods-layout-provider/src/main/java/com/veadan/folib/util/demo.txt";
        final PodSpec podSpec = resolvePodSpec(FileUtil.readString(path, StandardCharsets.UTF_8));
        System.out.println(podSpec);
    }
    
    /**
     *
     * @author xiaodong.wang
     * @email wangxiaodong@beyondcent.com
     * @date 2023/8/3 16:14
     * @since x.x.x
     */
    @Data
    public static class PodSpec 
    {
        private String name;
        private String version;
        private String license;
    }
}
