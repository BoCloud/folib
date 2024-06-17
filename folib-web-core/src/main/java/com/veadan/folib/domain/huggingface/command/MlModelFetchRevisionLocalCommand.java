package com.veadan.folib.domain.huggingface.command;

/**
 * 模型获取本地修订
 */

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import javax.annotation.Nullable;

import com.veadan.folib.domain.huggingface.model.RevisionData;
import com.veadan.folib.domain.huggingface.model.request.MlModelRequestContext;
import com.veadan.folib.domain.huggingface.utils.MlModelUtils;
import com.veadan.folib.domain.huggingface.utils.PathUtils;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import lombok.Generated;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static com.veadan.folib.domain.huggingface.constant.MlModelConstants.LEAD_FILE_NAME;

public class MlModelFetchRevisionLocalCommand {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(MlModelFetchRevisionLocalCommand.class);

    protected RepositoryPathResolver repositoryPathResolver;

    public MlModelFetchRevisionLocalCommand(RepositoryPathResolver repositoryPathResolver) {
        this.repositoryPathResolver = repositoryPathResolver;
    }

    public RevisionData fetchRevision(MlModelRequestContext requestContext) throws Exception {
        RevisionData revisionData;
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        log.debug("Received fetch revision request for repo {}, organization {}, model {}, revision {}", requestContext
                .getRepositoryId(), requestContext.getOrg(), requestContext.getModelName(), requestContext
                .getRevision());
        String latestLeadFilePath = getLatestLeadFilePath(requestContext);
        if (latestLeadFilePath == null) {
            return fetchLeadFileByGeneratedSha1Value(requestContext);
        }
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(requestContext.getStorageId(), requestContext.getRepositoryId(), latestLeadFilePath);
            InputStream leadStream = Files.newInputStream(repositoryPath);
            try {
                revisionData = MlModelUtils.createObjectMapper().readValue(leadStream, RevisionData.class);
                String leadFilePath = MlModelUtils.getFilePath(requestContext.getOrg(), requestContext.getModelName(), requestContext
                        .getRevision(), revisionData.getLastModified(), LEAD_FILE_NAME);
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
        return revisionData;
    }

    @Nullable
    private String getLatestLeadFilePath(MlModelRequestContext context) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(context.getStorageId(), context.getRepositoryId(), MlModelUtils.getModelRevisionPath(context));
        List<RepositoryPath> fileList = new ArrayList<>();
        try {
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    // 在这里可以处理目录（如果需要的话）
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    if (".folib_huggingface_model_info.json".equals(file.getFileName().toString())) {
                        fileList.add((RepositoryPath) file);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) {
                    // 处理无法访问的文件
                    log.error("访问文件失败: " + file.toString());
                    exc.printStackTrace();
                    return FileVisitResult.CONTINUE;
                }
            });

        } catch (IOException e) {
            log.error("访问文件失败: " + repositoryPath.toString());
            e.printStackTrace();
        }
        Date latestDate = null;
        RepositoryPath latestLeadFile = null;
        for (RepositoryPath leadFile : fileList) {
            String timeStampFolderByLeadFilePath = getTimeStampFolderByLeadFilePath(leadFile.getPath());
            if (StringUtils.isNotBlank(timeStampFolderByLeadFilePath)) {
                try {
                    Date currentDate = MlModelUtils.convertToDate(timeStampFolderByLeadFilePath);
                    if (latestDate == null || latestDate.before(currentDate)) {
                        latestDate = currentDate;
                        latestLeadFile = leadFile;
                    }
                } catch (Exception e) {
                    log.debug("Failed to update latest lead file path with: {}", timeStampFolderByLeadFilePath, e);
                }
            }
        }
        return (latestLeadFile != null) ? latestLeadFile.getPath() : null;
    }

    @Nullable
    public static String getTimeStampFolderByLeadFilePath(String leadFilePath) {
        if (leadFilePath == null) {
            throw new NullPointerException("leadFilePath is marked non-null but is null");
        }
        if (!PathUtils.isFolderPath(leadFilePath) &&
                PathUtils.getLastPathElement(leadFilePath).equals(LEAD_FILE_NAME)) {
            String timeStampFolderPath = PathUtils.getParent(leadFilePath);
            return PathUtils.getLastPathElement(timeStampFolderPath);
        }
        return null;
    }


    private RevisionData afterFailedToFetchLatestModelInfo(MlModelRequestContext context, String path, Exception e) throws Exception {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (path == null) {
            throw new NullPointerException("path is marked non-null but is null");
        }
        if (e == null) {
            throw new NullPointerException("e is marked non-null but is null");
        } else {
            log.error("Could not fetch a model info file for repoKey:{}, path:{}, message:{}", context.getRepositoryId(), path, e.getMessage());
            log.debug("Could not fetch a model info file for repoKey:{}, path:{}", context.getRepositoryId(), path, e);
            log.debug("Got 404 status while tried to fetch model info stream for repoKey:{}, path:{} about to try to fetch it by the internal generated revision as:{}", context.getRepositoryId(), path, context.getRevision());
            return fetchLeadFileByGeneratedSha1Value(context);
        }
    }


    private RevisionData fetchLeadFileByGeneratedSha1Value(MlModelRequestContext requestContext) throws RuntimeException {
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        log.debug("About to try to fetch model info object by search oh generated sha1 property for repoKey:{}, modelId:{}, revision:{}", requestContext
                .getRepositoryId(), requestContext.modelId(), requestContext.getRevision());
        String leadFilePath = getLeadFilePathByGeneratedSha1(requestContext, requestContext.getRevision());
        if (leadFilePath != null) {
            RevisionData revisionData = downloadRevisionData(requestContext, leadFilePath);
            if (StringUtils.isNotBlank(revisionData.getLastModified())) {
                String revisionFolder = MlModelUtils.getRevisionFolderByTimeStampLeadFilePath(requestContext, leadFilePath, revisionData
                        .getLastModified());
                requestContext.setVersionFolder(revisionFolder);
                return revisionData;
            }
            requestContext.setVersionFolder(leadFilePath.replace(LEAD_FILE_NAME, ""));
            log.debug("Found model info object for repoKey:{}, modelId:{}, revision:{} under the path:{}", requestContext.getRepositoryId(), requestContext.modelId(), requestContext.getRevision(), leadFilePath);
            return downloadRevisionData(requestContext, leadFilePath);
        }
        log.warn("Could not find model info object for repoKey:{}, modelId:{}, revision:{}", requestContext.getRepositoryId(), requestContext.modelId(), requestContext.getRevision());
        throw new RuntimeException("Could not find model info file");
    }

    private RevisionData downloadRevisionData(MlModelRequestContext requestContext, String leadFilePath) {
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        if (leadFilePath == null) {
            throw new NullPointerException("leadFilePath is marked non-null but is null");
        }
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(requestContext.getStorageId(), requestContext.getRepositoryId(), leadFilePath);
            InputStream leadStream = Files.newInputStream(repositoryPath);
            try {
                RevisionData revisionData = MlModelUtils.createObjectMapper().readValue(leadStream, RevisionData.class);
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
            log.warn("Failed to fetch revision data for for repo {}, organization {}, model {}, revision {}", requestContext
                    .getRepositoryId(), requestContext.getOrg(), requestContext.getModelName(), requestContext.getRevision());
            throw new RuntimeException("No artifacts found for revision " + requestContext.getRevision());
        }
    }

    private String getLeadFilePathByGeneratedSha1(MlModelRequestContext context, String generatedSha1) {
        String revisionPath =   MlModelUtils.getModelRevisionPath( context);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(context.getStorageId(), context.getRepositoryId(), revisionPath);
        List<RepositoryPath> fileList = new ArrayList<>();
        try {
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    // 在这里可以处理目录（如果需要的话）
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    if (".folib_huggingface_model_info.json".equals(file.getFileName().toString())) {
                        fileList.add((RepositoryPath) file);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) {
                    // 处理无法访问的文件
                    log.error("访问文件失败: " + file.toString());
                    exc.printStackTrace();
                    return FileVisitResult.CONTINUE;
                }
            });

        } catch (IOException e) {
            log.error("访问文件失败: " + repositoryPath.toString());
            e.printStackTrace();
        }
        return fileList.stream().filter(pa -> pa.getFileName().equals(".folib_huggingface_model_info.json")).findFirst().map(RepositoryPath::getPath).orElse(null);
    }
}

