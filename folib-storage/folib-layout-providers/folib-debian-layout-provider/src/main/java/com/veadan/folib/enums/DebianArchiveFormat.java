package com.veadan.folib.enums;

import com.veadan.folib.domain.DebianPackagesContext;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.DebianFormatWriter;
import com.veadan.folib.util.DebianUtils;
import com.veadan.folib.util.steam.MetadataStreamBz2;
import com.veadan.folib.util.steam.MetadataStreamGz;
import com.veadan.folib.util.steam.MetadataStreamLzma;
import com.veadan.folib.util.steam.MetadataStreamXz;
import com.veadan.folib.util.steam.StringInputStream;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;

/**
 * @author veadan
 * @since 2024-09-02 16:38
 */
@Slf4j
@Getter
public enum DebianArchiveFormat implements DebianFormatWriter {


    GZIP("Packages.gz", "gz") {
        public void writePackagesIndexFile(Repository repo, DebianPackagesContext packagesContext, File packagesFile, RepositoryPathResolver resolver, ArtifactManagementService artifactManagement) {
            try (InputStream contentStream = Files.newInputStream(packagesFile.toPath()); InputStream gzContentStream = new MetadataStreamGz(contentStream)) {
                writePackagesFile(packagesContext, this.fileName, gzContentStream, repo, resolver, artifactManagement);
            } catch (IOException e) {
                DebianArchiveFormat.log.error("Error while trying to write Packages.gz file");
                throw new RuntimeException(e);
            }
        }

        public InputStream createPackagesIndexInputStream(String packagesFilePlainTextContent) {
            InputStream contentStream = new StringInputStream(packagesFilePlainTextContent);
            return new MetadataStreamGz(contentStream);
        }
    },
    BZ2("Packages.bz2", "bz2") {
        public void writePackagesIndexFile(Repository repo, DebianPackagesContext packagesContext, File packagesFile, RepositoryPathResolver resolver, ArtifactManagementService artifactManagement) {
            try (InputStream contentStream = Files.newInputStream(packagesFile.toPath()); InputStream bz2ContentStream = new MetadataStreamBz2(contentStream)) {
                writePackagesFile(packagesContext, "Packages.bz2", bz2ContentStream, repo, resolver, artifactManagement);
            } catch (IOException e) {
                log.error("Error while trying to write Packages.bz2 file");
                throw new RuntimeException(e);
            }
        }

        public InputStream createPackagesIndexInputStream(String packagesFilePlainTextContent) {
            InputStream contentStream = new StringInputStream(packagesFilePlainTextContent);
            return new MetadataStreamBz2(contentStream);
        }
    },
    XZ("Packages.xz", "xz") {
        public void writePackagesIndexFile(Repository repo, DebianPackagesContext packagesContext, File packagesFile, RepositoryPathResolver resolver, ArtifactManagementService artifactManagement) {
            try (InputStream contentStream = Files.newInputStream(packagesFile.toPath()); InputStream xzContentStream = new MetadataStreamXz(contentStream)) {
                writePackagesFile(packagesContext, "Packages.xz", xzContentStream, repo, resolver, artifactManagement);
            } catch (IOException e) {
                log.error("Error while trying to write Packages.xz file");
                throw new RuntimeException(e);
            }
        }

        public InputStream createPackagesIndexInputStream(String packagesFilePlainTextContent) {
            InputStream contentStream = new StringInputStream(packagesFilePlainTextContent);
            return new MetadataStreamXz(contentStream);
        }
    },
    LZMA("Packages.lzma", "lzma") {
        public void writePackagesIndexFile(Repository repo, DebianPackagesContext packagesContext, File packagesFile, RepositoryPathResolver resolver, ArtifactManagementService artifactManagement) {
            try (InputStream contentStream = Files.newInputStream(packagesFile.toPath()); InputStream lzmaContentStream = new MetadataStreamLzma(contentStream)) {
                writePackagesFile(packagesContext, "Packages.lzma", lzmaContentStream, repo, resolver, artifactManagement);
            } catch (IOException e) {
                log.error("Error while trying to write Packages.lzma file");
                throw new RuntimeException(e);
            }
        }

        public InputStream createPackagesIndexInputStream(String packagesFilePlainTextContent) {
            InputStream contentStream = new StringInputStream(packagesFilePlainTextContent);
            return new MetadataStreamLzma(contentStream);
        }
    };

    public final String fileName;
    public final String ext;


    DebianArchiveFormat(String fileName, String extention) {
        this.fileName = fileName;
        this.ext = extention;
    }

    public static void writePackagesFile(DebianPackagesContext packagesContext, String packagesFileName, InputStream input, Repository repo, RepositoryPathResolver resolver, ArtifactManagementService artifactManagement) throws IOException {
        String packagesPath = DebianUtils.pathToPackagesFile(packagesContext, packagesFileName);
        RepositoryPath repositoryPath = resolver.resolve(repo, packagesPath);
        artifactManagement.store(repositoryPath, input);
        log.debug("Writing {} to path {} for coordinates {}", packagesFileName, packagesPath, DebianUtils.print(packagesContext));
        log.debug("Finished deployment of path {}. ", packagesPath);
    }

    public static DebianArchiveFormat from(String format) {
        String rawExt = format.replace(".", "");
        DebianArchiveFormat[] formats = values();
        for (DebianArchiveFormat debianArchiveFormat : formats) {
            if (debianArchiveFormat.ext.equalsIgnoreCase(rawExt)) {
                return debianArchiveFormat;
            }
        }
        return null;
    }

    public static int compare(DebianArchiveFormat left, DebianArchiveFormat right) {
        return left.compareTo(right);
    }
}
