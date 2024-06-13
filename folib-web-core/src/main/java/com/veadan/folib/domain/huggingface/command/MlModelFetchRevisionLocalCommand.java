package com.veadan.folib.domain.huggingface.command;

/**
 * 模型获取本地修订
 */

import java.io.IOException;
import java.io.InputStream;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import javax.annotation.Nullable;

import com.veadan.folib.domain.huggingface.model.RevisionData;
import com.veadan.folib.domain.huggingface.model.request.MlModelRequestContext;
import com.veadan.folib.domain.huggingface.utils.MlModelUtils;
import com.veadan.folib.domain.huggingface.utils.PathUtils;
import lombok.Generated;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MlModelFetchRevisionLocalCommand {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(MlModelFetchRevisionLocalCommand.class);



    public MlModelFetchRevisionLocalCommand() {
    }

    public RevisionData fetchRevision(MlModelRequestContext requestContext) throws Exception {
        RevisionData revisionData;
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        log.debug("Received fetch revision request for repo {}, organization {}, model {}, revision {}", requestContext
                .getRepositoryId(), requestContext.getOrg(), requestContext.getModelName(), requestContext
                .getRevision() );
        String latestLeadFilePath = getLatestLeadFilePath(requestContext);
        if (latestLeadFilePath == null) {
            return fetchLeadFileByGeneratedSha1Value(requestContext);
        }
        try {
            InputStream leadStream = null;//this.downloadService.getStream(requestContext.getRepoKey(), latestLeadFilePath);
            try {
                revisionData = (RevisionData)MlModelUtils.createObjectMapper().readValue(leadStream, RevisionData.class);
                String leadFilePath = MlModelUtils.getFilePath(requestContext.getOrg(), requestContext.getModelName(), requestContext
                        .getRevision(), revisionData.getLastModified(), ".jfrog_huggingface_model_info.json");
                String revisionFolder = MlModelUtils.getRevisionFolderByTimeStampLeadFilePath(requestContext, leadFilePath, revisionData
                        .getLastModified());
                requestContext.setVersionFolder(revisionFolder);
                if (leadStream != null) {
                    leadStream.close();
                }
            } catch (Throwable throwable) {
                if (leadStream != null) {
                    try {
                        leadStream.close();
                    } catch (Throwable throwable1) {
                        throwable.addSuppressed(throwable1);
                    }
                }
                throw throwable;
            }
        } catch (Exception e) {
            return afterFailedToFetchLatestModelInfo(requestContext, latestLeadFilePath, e);
        }
        //} catch (IOException e) {
        //    log.warn("Failed to fetch revision data for for repo {}, organization {}, model {}, revision {}",  requestContext
        //            .getRepositoryId(), requestContext.getOrg(), requestContext.getModelName(), requestContext.getRevision() );
        //    throw new RuntimeException("No artifacts found for revision " + requestContext.getRevision());
        //}
        return revisionData;
    }

    @Nullable
    private String getLatestLeadFilePath( MlModelRequestContext context) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        // TODO: 2024/6/6
        //List<PackageArtifact> leadFiles = this.searchService.findArtifactsChildren(context.getRepoKey(), MlModelUtils.getModelRevisionPath(context)).filter(artifact -> ".jfrog_huggingface_model_info.json".equals(artifact.getName())).toList();
        //PackageArtifact latestLeadFile = null;
        //Date latestDate = null;
        //for (PackageArtifact leadFile : leadFiles) {
        //    String timeStampFolderByLeadFilePath = getTimeStampFolderByLeadFilePath(leadFile.getPath());
        //    if (StringUtils.isNotBlank(timeStampFolderByLeadFilePath))
        //        try {
        //            Date currentDate = MlModelUtils.convertToDate(timeStampFolderByLeadFilePath);
        //            if (latestDate == null || latestDate.before(currentDate)) {
        //                latestDate = currentDate;
        //                latestLeadFile = leadFile;
        //            }
        //        } catch (Exception e) {
        //            log.debug("Failed to update latest lead file path with: {}", timeStampFolderByLeadFilePath, e);
        //        }
        //}
        return null;//(latestLeadFile != null) ? latestLeadFile.getPath() : null;
    }

    @Nullable
    public static String getTimeStampFolderByLeadFilePath( String leadFilePath) {
        if (leadFilePath == null) {
            throw new NullPointerException("leadFilePath is marked non-null but is null");
        }
        if (!PathUtils.isFolderPath(leadFilePath) &&
                PathUtils.getLastPathElement(leadFilePath).equals(".jfrog_huggingface_model_info.json")) {
            String timeStampFolderPath = PathUtils.getParent(leadFilePath);
            return PathUtils.getLastPathElement(timeStampFolderPath);
        }
        return null;
    }

    
    private RevisionData afterFailedToFetchLatestModelInfo( MlModelRequestContext context,  String path,  Exception e) throws Exception  {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (path == null) {
            throw new NullPointerException("path is marked non-null but is null");
        }
        if (e == null) {
            throw new NullPointerException("e is marked non-null but is null");
        }
        // TODO: 2024/6/6
        //if (e.getStatus() == 404) {
        //    log.debug("Got 404 status while tried to fetch model info stream for repoKey:{}, path:{} about to try to fetch it by the internal generated revision as:{}", new Object[] { context
        //
        //            .getRepoKey(), path, context.getRevision() });
        //    return fetchLeadFileByGeneratedSha1Value(context);
        //}
        log.error("Could not fetch a model info file for repoKey:{}, path:{}, message:{}",  context.getRepositoryId(), path, e.getMessage() );
        log.debug("Could not fetch a model info file for repoKey:{}, path:{}",  context.getRepositoryId(), path, e );
        throw e;
    }

    
    private RevisionData fetchLeadFileByGeneratedSha1Value( MlModelRequestContext requestContext) throws RuntimeException {
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        log.debug("About to try to fetch model info object by search oh generated sha1 property for repoKey:{}, modelId:{}, revision:{}", requestContext
                .getRepositoryId(), requestContext.modelId(), requestContext.getRevision() );
        String leadFilePath = getLeadFilePathByGeneratedSha1(requestContext.getRepositoryId(), requestContext.getRevision());
        if (leadFilePath != null) {
            RevisionData revisionData = downloadRevisionData(requestContext, leadFilePath);
            if (StringUtils.isNotBlank(revisionData.getLastModified())) {
                String revisionFolder = MlModelUtils.getRevisionFolderByTimeStampLeadFilePath(requestContext, leadFilePath, revisionData
                        .getLastModified());
                requestContext.setVersionFolder(revisionFolder);
                return revisionData;
            }
            requestContext.setVersionFolder(leadFilePath.replace(".jfrog_huggingface_model_info.json", ""));
            log.debug("Found model info object for repoKey:{}, modelId:{}, revision:{} under the path:{}",  requestContext.getRepositoryId(), requestContext.modelId(), requestContext.getRevision(), leadFilePath );
            return downloadRevisionData(requestContext, leadFilePath);
        }
        log.warn("Could not find model info object for repoKey:{}, modelId:{}, revision:{}", requestContext.getRepositoryId(), requestContext.modelId(), requestContext.getRevision() );
        throw new RuntimeException("Could not find model info file");
    }

    private RevisionData downloadRevisionData( MlModelRequestContext requestContext,  String leadFilePath) {
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        if (leadFilePath == null) {
            throw new NullPointerException("leadFilePath is marked non-null but is null");
        }
        try {
            InputStream leadStream = null;//this.downloadService.getStream(requestContext.getRepoKey(), leadFilePath);
            try {
                RevisionData revisionData = (RevisionData) MlModelUtils.createObjectMapper().readValue(leadStream, RevisionData.class);
                if (leadStream != null) {
                    leadStream.close();
                }
                return revisionData;
            } catch (Throwable throwable) {
                if (leadStream != null) {
                    try {
                        leadStream.close();
                    } catch (Throwable throwable1) {
                        throwable.addSuppressed(throwable1);
                    }
                }
                throw throwable;
            }
        } catch (IOException e) {
            log.warn("Failed to fetch revision data for for repo {}, organization {}, model {}, revision {}",  requestContext
                    .getRepositoryId(), requestContext.getOrg(), requestContext.getModelName(), requestContext.getRevision());
            throw new RuntimeException("No artifacts found for revision " + requestContext.getRevision());
        }
    }

    @Nullable
    private String getLeadFilePathByGeneratedSha1(String repoKey, String generatedSha1) {
        //PackageSearchContext searchContext = new PackageSearchContext(repoKey, "");
        //Stream<PackageArtifact> filesWithProp = this.searchService.findArtifactsByPropKeyAndVal(searchContext, Map.of("huggingfaceml.generated.revision.sha1", generatedSha1));
        //return filesWithProp.filter(pa -> pa.getName().equals(".jfrog_huggingface_model_info.json")).findFirst()
        //        .map(PackageArtifact::getPath).orElse(null);
        return "";
    }
}

