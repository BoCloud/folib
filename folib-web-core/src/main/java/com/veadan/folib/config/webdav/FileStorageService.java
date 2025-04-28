package com.veadan.folib.config.webdav;

import com.veadan.folib.domain.DirectoryListing;
import io.milton.resource.Resource;
import io.milton.http.exceptions.BadRequestException;

import java.io.IOException;
import java.io.InputStream;
import java.util.Date;
import java.util.List;

/**
 * @author huayanjun
 * @since 2025-03-09 16:14
 */
public interface FileStorageService {

    byte[] getFileContent(String path) throws IOException;
    void saveFileContent(String path, byte[] content) throws IOException;
    DirectoryListing listDirectory(String path) throws IOException;
    boolean isDirectory(String path);
    boolean exists(String path);
    Long getContentLength(String path);
    void copyFile(String sourcePath, String destPath) throws BadRequestException;
    void moveFile(String sourcePath, String destPath) throws BadRequestException;
    void deleteFile(String path) throws IOException;
    Date getModifiedDate(String path);
    void createDirectory(String path) throws IOException;
    void saveFileContentFromStream(String path, InputStream inputStream, Long length) throws IOException;
    InputStream getFileInputStream(String path) throws IOException;
    void saveFileInputStream(String path,InputStream inputStream);

}
