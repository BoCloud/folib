package com.veadan.folib.artifact.archive;

import com.veadan.folib.providers.io.RepositoryPath;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

/**
 * @author veadan
 */
public enum ZipArchiveListingFunction
        implements ArchiveListingFunction {

    /**
     * 实例
     */
    INSTANCE;

    @Override
    public Set<String> listFilenames(final RepositoryPath path)
            throws IOException {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             ArchiveInputStream ais = new ZipArchiveInputStream(bis)) {
            return getEntryNames(ais);
        }
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath path, String fileName) throws IOException {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             ArchiveInputStream ais = new ZipArchiveInputStream(bis)) {
            return getContentByFileName(ais, fileName);
        }
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath repositoryPath, Path path, String fileName) throws IOException {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             ArchiveInputStream ais = new ZipArchiveInputStream(bis)) {
            return getContentByFileName(ais, fileName);
        }
    }

    @Override
    public byte[] getContentByEqualsFileName(RepositoryPath repositoryPath, Path path, String fileName) throws IOException {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             ArchiveInputStream ais = new ZipArchiveInputStream(bis)) {
            return getContentByEqualsFileName(ais, fileName);
        }
    }

    @Override
    public boolean supports(final RepositoryPath path) {
        final Path fileName = path.getFileName();
        return fileName != null && fileName.toString().endsWith("zip");
    }
}
