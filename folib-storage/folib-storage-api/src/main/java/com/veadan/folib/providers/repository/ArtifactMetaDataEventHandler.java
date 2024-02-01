//package com.veadan.folib.providers.repository;
//
//import com.veadan.folib.artifact.AsyncArtifactEntryHandler;
//import com.veadan.folib.domain.Artifact;
//import com.veadan.folib.domain.ArtifactEntity;
//import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
//import com.veadan.folib.providers.io.RepositoryPath;
//import com.veadan.folib.util.LocalDateTimeInstance;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.stereotype.Component;
//
//import java.io.IOException;
//
//@Component
//@Slf4j
//public class ArtifactMetaDataEventHandler extends AsyncArtifactEntryHandler {
//
//    public ArtifactMetaDataEventHandler() {
//        super(ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_UPDATE);
//    }
//
//    @Override
//    protected Artifact handleEvent(RepositoryPath repositoryPath) throws IOException {
//        Artifact artifactEntry = repositoryPath.getArtifactEntry();
//        if (artifactEntry == null) {
//            log.warn("No [{}] for [{}].",
//                    Artifact.class.getSimpleName(),
//                    repositoryPath);
//
//            return null;
//        }
//        Artifact updateArtifactEntry = new ArtifactEntity(artifactEntry.getNativeId(), artifactEntry.getStorageId(), artifactEntry.getRepositoryId(), artifactEntry.getUuid(), artifactEntry.getArtifactCoordinates());
//        updateArtifactEntry.setMetadata(artifactEntry.getMetadata());
//        log.info("存储空间：{} 仓库：{} 制品：{} 当前元数据：{}", updateArtifactEntry.getStorageId(), updateArtifactEntry.getRepositoryId(), updateArtifactEntry.getArtifactPath(), updateArtifactEntry.getMetadata());
//        updateArtifactEntry.setLastUpdated(LocalDateTimeInstance.now());
//        return updateArtifactEntry;
//    }
//
//}
