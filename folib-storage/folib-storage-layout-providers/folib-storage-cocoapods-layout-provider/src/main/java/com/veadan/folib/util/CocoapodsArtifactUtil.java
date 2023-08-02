package com.veadan.folib.util;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;

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
    private static final Pattern PODSPEC_HEAD_PATTERN = Pattern.compile("Pod\\:\\:Spec\\.new\\s+?do\\s+?\\|(.*?)\\|");
    
    /**
     * 编辑Pod.tar.gz文件输入流中的*.podspec文件*.source属性，并另存为Pod.tar.gz文件
     * @param inputStream Pod.tar.gz文件输入流
     * @param newSourceUrl 新的SourceUrl（非 Pod.tar.gz Url，例如：http://10.10.33.149:8081/artifactory/api/pods/Cocoapad-Local/pod/pkg/AFNetworking/4.0.1 ）
     * @param newTarGzFilePath 新的Pod.tar.gz存储路径
     * @return 结果
     * @since x.x.x
     */
    public static boolean replacePodspecSourceSaveAsTarGzFile(InputStream inputStream, String newSourceUrl, String newTarGzFilePath)
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
                        final Matcher matcher = PODSPEC_HEAD_PATTERN.matcher(podspecContent);
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
}
