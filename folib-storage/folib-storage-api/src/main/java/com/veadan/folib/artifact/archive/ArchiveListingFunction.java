package com.veadan.folib.artifact.archive;

import com.veadan.folib.providers.io.RepositoryPath;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.io.IOUtils;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.Set;

/**
 * @author veadan
 */
//@FunctionalInterface
public interface ArchiveListingFunction {

    Set<String> listFilenames(RepositoryPath path)
            throws IOException;

    String getContentByFileName(RepositoryPath path, String fileName) throws IOException;

    default String getContentByFileName(final ArchiveInputStream archiveInputStream, String fileName) throws IOException {
        ArchiveEntry entry;
        while ((entry = archiveInputStream.getNextEntry()) != null) {
            if (entry.getName().equals(fileName)) {
                ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                try (byteArrayOutputStream) {
                    IOUtils.copy(archiveInputStream, byteArrayOutputStream);
                } catch (IOException ex) {
                    throw new IOException(ex);
                }
                return new String(byteArrayOutputStream.toByteArray(), StandardCharsets.UTF_8);
            }
        }
        return "";
    }

    default Set<String> getEntryNames(final ArchiveInputStream archiveInputStream)
            throws IOException {
        final Set<String> result = new HashSet<>();
        ArchiveEntry entry;
        while ((entry = archiveInputStream.getNextEntry()) != null) {
            result.add(entry.getName());
        }
        return result;
    }

    default boolean supports(RepositoryPath path) {
        return true;
    }
}
