package com.veadan.folib.indexer;

import com.veadan.folib.domain.DebianMetadata;
import com.veadan.folib.domain.DebianPackagesContext;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.DebianUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.compressors.CompressorException;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * @author huayanjun
 * @since 2024-09-04 15:30
 */
@Slf4j
public class DebianPackagesMetadataIndexer extends DebianPackagesMetadataIndexerBase {

    private final Set<String> forcedArchitectures = Collections.emptySet();

    public DebianPackagesMetadataIndexer(Repository repo, RepositoryPathResolver resolver, ArtifactManagementService artifactManagementService) {
        super(repo, resolver, artifactManagementService);
    }

    public void indexPackages(List<DebianMetadata> mds, DebianPackagesContext packagesContext) {
        String coordinates = DebianUtils.print(packagesContext);
        log.debug("Writing Packages index for coordinates {}", coordinates);
        mds = DebianUtils.uniqueSortedMetadataList(mds);
        if (!DebianUtils.shouldDeletePackagesOfContext(mds, packagesContext, this.forcedArchitectures)) {
            File tmpFile = null;
            try {
                log.debug("Indexing coordinates: {}", coordinates);
                tmpFile = this.writePackagesIndex(mds, coordinates);
                this.writePackagesFileContentToRepo(packagesContext, tmpFile);
            } catch (CompressorException | IOException e) {
                throw new RuntimeException(e);
            } finally {
                DebianUtils.deleteTempFile(tmpFile);
            }
        } else {
            log.debug("Index for {} will be - no Debian packages exist for it and it's not a forced architecture.", coordinates);
        }
    }

    private File writePackagesIndex(Iterable<DebianMetadata> metadatas, String coordinates) throws IOException {
        File tempPackagesFile = this.getTempPackagesFilePath("134");
        try (Writer writer = new BufferedWriter(new FileWriter(tempPackagesFile), 8192)) {
            for (DebianMetadata metadata : metadatas) {
                this.writeMetadataEntry(writer, metadata);
            }
            writer.flush();
        }
        log.info("Writing Packages index of coordinates {} to fs temp took", coordinates);
        return tempPackagesFile;
    }

    private void writeMetadataEntry(Writer writer, DebianMetadata metadata) throws IOException {
        log.trace("Writing Debian Packages index entry for artifact '{}'", metadata.artifactRelativePath);
        writer.write(metadata.controlFileContent.trim() + "\n");
        writer.flush();
    }


}
