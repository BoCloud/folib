package com.veadan.folib.util;

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
