package com.veadan.folib.utils;

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
 * @author leipenghui
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
