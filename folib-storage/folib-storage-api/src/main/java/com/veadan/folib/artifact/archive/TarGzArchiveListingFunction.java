package com.veadan.folib.artifact.archive;

import com.veadan.folib.providers.io.RepositoryPath;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.io.IOUtils;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

/**
 * @author veadan
 */
public enum TarGzArchiveListingFunction
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
             GzipCompressorInputStream gzi = new GzipCompressorInputStream(bis);
             ArchiveInputStream ais = new TarArchiveInputStream(gzi)) {
            return getEntryNames(ais);
        }
    }

    @Override
    public String getContentByFileName(RepositoryPath path, String fileName) throws IOException {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             GzipCompressorInputStream gzi = new GzipCompressorInputStream(bis);
             ArchiveInputStream ais = new TarArchiveInputStream(gzi)) {
            return getContentByFileName(ais, fileName);
        }
    }

    @Override
    public String getContentByFileName(RepositoryPath repositoryPath, Path path, String fileName) throws IOException {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             GzipCompressorInputStream gzi = new GzipCompressorInputStream(bis);
             ArchiveInputStream ais = new TarArchiveInputStream(gzi)) {
            return getContentByFileName(ais, fileName);
        }
    }

    @Override
    public boolean supports(final RepositoryPath path) {
        final Path fileName = path.getFileName();
        return fileName != null && (fileName.toString().endsWith("tar.gz") || fileName.toString().endsWith("tgz"));
    }

}
