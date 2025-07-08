package com.folib.util;

import com.folib.cache.DebianPackagesMetadataCache;
import com.folib.constant.DebianConstant;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactWithMetadata;
import com.folib.domain.DebianMetadata;
import com.folib.indexer.DebianIncrementalIndexer;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang.StringUtils;

import java.io.InputStream;
import java.util.function.Function;

/**
 * @author veadan
 * @since 2024-09-03 17:27
 */
@Slf4j
public class ArtifactWithMetadataTransformer implements Function<Artifact, ArtifactWithMetadata> {

    public static final String FAIL_REASON = "deb.index.status";
    private final Repository repo;
    private final DebianPackagesMetadataCache cache;

    private final DebianIncrementalIndexer debianIncrementalIndexer;

    public ArtifactWithMetadataTransformer(Repository repo, DebianPackagesMetadataCache cache,DebianIncrementalIndexer debianIncrementalIndexer) {
        this.repo = repo;
        this.cache = cache;
        this.debianIncrementalIndexer=debianIncrementalIndexer;
    }

    @Override
    public ArtifactWithMetadata apply(Artifact artifact) {
        DebianMetadata metadata = null;
        ArtifactWithMetadata artifactWithMetadata = null;
        if (this.cache != null) {
            metadata = this.cache.get(artifact, this.repo);
        }
        if (metadata == null) {
            metadata = this.extractAndCacheMetadata(artifact);
        }
        if (metadata != null && StringUtils.isNotBlank(metadata.packageName)) {
            artifactWithMetadata = new ArtifactWithMetadata(artifact, metadata);
        } else {
            log.error("Failed to retrieve metadata from artifact: {}", artifact.getArtifactPath());
        }
        return artifactWithMetadata;
    }

    private DebianMetadata extractAndCacheMetadata(Artifact artifact) {
        DebianMetadata metadata = null;
        try (InputStream is=debianIncrementalIndexer.getPackageByRepo(this.repo,artifact.getArtifactPath())){
            metadata = DebianUtils.extract(is);
            if (this.cache != null && metadata != null) {
                DebianUtils.appendOrReplace(metadata, DebianConstant.CONTROL_FILENAME,metadata.getFilename(),artifact.getArtifactPath());
                DebianUtils.appendOrReplace(metadata, DebianConstant.CONTROL_SIZE,null,String.valueOf(artifact.getSizeInBytes()));
                DebianUtils.appendOrReplace(metadata, DebianConstant.CONTROL_MD5SUM,metadata.getMd5sum(),artifact.getChecksums().get(MessageDigestAlgorithms.MD5));
                DebianUtils.appendOrReplace(metadata, DebianConstant.CONTROL_SHA256,metadata.getSha256(),artifact.getChecksums().get(MessageDigestAlgorithms.SHA_256));
                this.cache.put(artifact, metadata);
            }
        } catch (Exception e) {
            String err = "Failed to extract and cache metadata for artifact " + artifact.getArtifactPath() + ": ";
            log.error(err + e.getMessage());
            log.debug(err, e);
        }
        return metadata;
    }
}
