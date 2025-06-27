package com.veadan.folib.services.impl;

import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;

/**
 * @author veadan
 * @date 2024/2/18 11:43
 */
public class FileStreamMultipartFile  implements MultipartFile {
    private final File file;
    private final String name;
    private final String originalFilename;
    private final String contentType;

    public FileStreamMultipartFile(File file, String name, String originalFilename, String contentType) {
        this.file = file;
        this.name = name;
        this.originalFilename = originalFilename;
        this.contentType = contentType;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getOriginalFilename() {
        return originalFilename;
    }

    @Override
    public String getContentType() {
        return contentType;
    }

    @Override
    public boolean isEmpty() {
        return file.length() == 0;
    }

    @Override
    public long getSize() {
        return file.length();
    }

    @Override
    public byte[] getBytes() throws IOException {
        // 为避免内存溢出，推荐不实现此方法，或者在文档中明确警告可能的内存问题
        throw new UnsupportedOperationException("getBytes not supported due to potential memory issues.");
    }

    @Override
    public InputStream getInputStream() throws IOException {
        return new FileInputStream(file);
    }

    @Override
    public void transferTo(File dest) throws IOException, IllegalStateException {
        Files.copy(file.toPath(), dest.toPath());
    }
}
