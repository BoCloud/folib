package com.folib.index.indexer;

import com.google.common.annotations.VisibleForTesting;
import com.folib.index.model.Index;
import com.folib.index.model.MetaYaml;
import com.folib.index.utils.CondaPathUtils;
import com.folib.index.utils.CondaUtils;
import com.folib.index.utils.JsonUtils;
import lombok.Generated;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream;
import org.apache.commons.compress.compressors.zstandard.ZstdCompressorInputStream;
import org.apache.commons.io.IOUtils;
import org.springframework.stereotype.Component;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.validation.constraints.NotNull;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;



@Slf4j
@Component
public class CondaMetadataExtractor {

    @Generated
    private static final String EXPECTED_TAR_ZST_FILE_REGEXP = "info-.+\\.tar\\.zst";
    private static final String INDEX_JSON = "index.json";

    @Nullable
    public Index extract(@NonNull String repoKey, @NonNull String artifactName) {
        if (repoKey == null) {
            throw new NullPointerException("repoKey is marked non-null but is null");
        } else if (artifactName == null) {
            throw new NullPointerException("artifact is marked non-null but is null");
        } else {
            Index index = (Index)this.getIndex(repoKey, artifactName, "index.json", Index.class,
                    MetadataFormat.JSON);
            this.supplementTimestampAndDependsFieldsIfNeeded(index);
            return index;
        }
    }

    @Nullable
    <T> T getIndex(@NonNull String repoKey,
                   @NonNull String artifactName,
                   @NonNull String archiveEntryName,
                   @NonNull Class<T> model,
                   @NonNull MetadataFormat metadataFormat) {
        if (repoKey == null) {
            throw new NullPointerException("repoKey is marked non-null but is null");
        } else if (artifactName == null) {
            throw new NullPointerException("artifactName is marked non-null but is null");
        } else if (archiveEntryName == null) {
            throw new NullPointerException("archiveEntryName is marked non-null but is null");
        } else if (model == null) {
            throw new NullPointerException("model is marked non-null but is null");
        } else if (metadataFormat == null) {
            throw new NullPointerException("metadataFormat is marked non-null but is null");
        } else {
            if (artifactName.endsWith(".tar.bz2")) {
                return (T)this.readArchiveEntry(repoKey, artifactName, archiveEntryName, model, metadataFormat);
            } else if (artifactName.endsWith(".conda")) {
                return (T)this.readCondaFileArchiveEntry(repoKey, artifactName, archiveEntryName, model,
                        metadataFormat);
            } else {
                return null;
            }
        }
    }

    @Nullable
    private <T> T readCondaFileArchiveEntry(String repoKey,
                                            @NonNull String artifactName,
                                            @NonNull String archiveEntryName,
                                            @NonNull Class<T> model,
                                            @NonNull MetadataFormat metadataFormat) {
        if (artifactName == null) {
            throw new NullPointerException("artifact is marked non-null but is null");
        } else if (archiveEntryName == null) {
            throw new NullPointerException("archiveEntryName is marked non-null but is null");
        } else if (model == null) {
            throw new NullPointerException("model is marked non-null but is null");
        } else if (metadataFormat == null) {
            throw new NullPointerException("metadataFormat is marked non-null but is null");
        } else {
            try {
                Object var10;
                String artifactPath = repoKey + "/" + artifactName;
                Path path = Path.of(artifactPath);
                try (InputStream inputStream = new BufferedInputStream(Files.newInputStream(path))) {
                    try (ZipArchiveInputStream zipArchiveInputStream = new ZipArchiveInputStream(inputStream, "UTF-8", true, true)) {
                        String entryName;
                        do {
                            ArchiveEntry packageArchiveEntry;
                            if ((packageArchiveEntry = zipArchiveInputStream.getNextEntry()) == null) {
                                return null;
                            }

                            entryName = CondaPathUtils.getFileName(packageArchiveEntry.getName());
                        } while(!entryName.matches("info-.+\\.tar\\.zst"));

                        var10 = this.readTarZstFile(artifactName, archiveEntryName, model, metadataFormat, entryName,
                                zipArchiveInputStream);
                    }
                }

                return (T)var10;
            } catch (Exception e) {
                return null;
            }
        }
    }

    @Nullable
    private <T> T readTarZstFile(@NotNull String artifactName,
                                 @NotNull String archiveEntryName,
                                 @NotNull Class<T> model,
                                 @NotNull MetadataFormat metadataFormat,
                                 String entryName,
                                 ZipArchiveInputStream zipArchiveInputStream)
            throws IOException {
        ZstdCompressorInputStream zstdCompressorInputStream = new ZstdCompressorInputStream(zipArchiveInputStream);
        TarArchiveInputStream tarArchiveInputStream = new TarArchiveInputStream(zstdCompressorInputStream);
        return (T)this.readTarArchive(tarArchiveInputStream, artifactName, archiveEntryName, model, metadataFormat);
    }

    @Nullable
    <T> T readArchiveEntry(@NonNull String repoKey,
                           @NonNull String artifactName,
                           @NonNull String archiveEntryName,
                           @NonNull Class<T> model,
                           @NonNull MetadataFormat metadataFormat) {
        if (repoKey == null) {
            throw new NullPointerException("repoKey is marked non-null but is null");
        } else if (artifactName == null) {
            throw new NullPointerException("artifact is marked non-null but is null");
        } else if (archiveEntryName == null) {
            throw new NullPointerException("archiveEntryName is marked non-null but is null");
        } else if (model == null) {
            throw new NullPointerException("model is marked non-null but is null");
        } else if (metadataFormat == null) {
            throw new NullPointerException("metadataFormat is marked non-null but is null");
        } else {
            try {
                Object var8;
                String artifactPath = repoKey + "/" + artifactName;
                Path path = Path.of(artifactPath);
                try (
                        InputStream inputStream = Files.newInputStream(path);
                        TarArchiveInputStream packageArchiveStream = new TarArchiveInputStream(new BZip2CompressorInputStream(inputStream));
                ) {
                    var8 = this.readTarArchive(packageArchiveStream, artifactPath, archiveEntryName, model,
                            metadataFormat);
                }

                return (T)var8;
            } catch (Exception e) {
                return null;
            }
        }
    }

    @Nullable
    private <T> T readTarArchive(@NonNull TarArchiveInputStream packageArchiveStream,
                                 @NonNull String artifactPath,
                                 @NonNull String archiveEntryName,
                                 @NonNull Class<T> model,
                                 @NonNull MetadataFormat metadataFormat)
            throws IOException {
        if (packageArchiveStream == null) {
            throw new NullPointerException("packageArchiveStream is marked non-null but is null");
        } else if (artifactPath == null) {
            throw new NullPointerException("artifact is marked non-null but is null");
        } else if (archiveEntryName == null) {
            throw new NullPointerException("archiveEntryName is marked non-null but is null");
        } else if (model == null) {
            throw new NullPointerException("model is marked non-null but is null");
        } else if (metadataFormat == null) {
            throw new NullPointerException("metadataFormat is marked non-null but is null");
        } else {
            T deserializedArchiveEntryWithMinDepth = null;
            int minDepth = Integer.MAX_VALUE;

            ArchiveEntry packageArchiveEntry;
            while((packageArchiveEntry = packageArchiveStream.getNextEntry()) != null) {
                String entryName = packageArchiveEntry.getName();
                String entryFileName = CondaPathUtils.getFileName(entryName);
                int depth = CondaUtils.getDepth(entryName);
                if (this.foundInMinDepth(packageArchiveStream, archiveEntryName, depth, minDepth, entryFileName, packageArchiveEntry)) {
                    minDepth = depth;
                    deserializedArchiveEntryWithMinDepth =
                            (T)this.deserializeArchiveEntry(IOUtils.toString(packageArchiveStream,
                                    Charset.defaultCharset()), artifactPath, model, metadataFormat);
                }
            }

            return deserializedArchiveEntryWithMinDepth;
        }
    }

    private boolean foundInMinDepth(@Nonnull TarArchiveInputStream packageArchiveStream,
                                    @Nonnull String archiveEntryName,
                                    int depth,
                                    int minDepth,
                                    String entryFileName,
                                    ArchiveEntry packageArchiveEntry) {
        return depth < minDepth && archiveEntryName.equals(entryFileName) && packageArchiveStream.canReadEntryData(packageArchiveEntry);
    }

    @Nullable
    <T> T deserializeArchiveEntry(@NonNull String entryStreamStr, @NonNull String artifactName, @NonNull Class<T> clazz, @NonNull MetadataFormat metadataFormat) {
        if (entryStreamStr == null) {
            throw new NullPointerException("entryStreamStr is marked non-null but is null");
        } else if (artifactName == null) {
            throw new NullPointerException("artifactName is marked non-null but is null");
        } else if (clazz == null) {
            throw new NullPointerException("clazz is marked non-null but is null");
        } else if (metadataFormat == null) {
            throw new NullPointerException("metadataFormat is marked non-null but is null");
        } else {
            try {
                if (metadataFormat == MetadataFormat.JSON) {
                    return (T) JsonUtils.getInstance().readValue(entryStreamStr, clazz);
                }

                if (metadataFormat == MetadataFormat.YAML) {
                    return (T)clazz.cast(this.parseMetadataManually(entryStreamStr));
                }
            } catch (Exception e) {
                log.debug(e.getMessage());
            }

            return null;
        }
    }

    private MetaYaml parseMetadataManually(String entryStreamStr) {
        MetaYaml metaYaml = new MetaYaml();
        metaYaml.setBuild(new HashMap());
        boolean isInBuildSection = false;
        int buildSectionIdent = -1;
        String[] splitEntryStreamStr = entryStreamStr.split("\n");

        for(String line : splitEntryStreamStr) {
            if (line.startsWith("build:")) {
                isInBuildSection = true;
            } else if (isInBuildSection) {
                int lineIndentation = CondaUtils.countLeadingSpaces(line);
                if (buildSectionIdent == -1) {
                    buildSectionIdent = lineIndentation;
                }

                if (lineIndentation == 0 || lineIndentation != buildSectionIdent) {
                    break;
                }

                CondaUtils.splitLine(line, metaYaml);
            }
        }

        return metaYaml;
    }

    @VisibleForTesting
    void supplementTimestampAndDependsFieldsIfNeeded(Index index) {
        if (index != null) {
            if (index.getTimestamp() == null && index.getMtime() != null) {
                index.setTimestamp(index.getMtime());
            }

            if (index.getDepends() == null && index.getRequires() != null) {
                index.setDepends(index.getRequires());
            }

        }
    }

    static enum MetadataFormat {
        JSON,
        YAML;

        private MetadataFormat() {
        }
    }
}
