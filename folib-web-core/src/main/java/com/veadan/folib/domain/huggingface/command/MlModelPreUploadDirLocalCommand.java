package com.veadan.folib.domain.huggingface.command;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import javax.annotation.Nonnull;

import com.veadan.folib.domain.huggingface.constant.MlModelSystemProperties;
import com.veadan.folib.domain.huggingface.model.request.*;
import com.veadan.folib.domain.huggingface.utils.MlModelUtils;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import lombok.Generated;
import lombok.NonNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MlModelPreUploadDirLocalCommand {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(MlModelPreUploadDirLocalCommand.class);


    //private final PackageHandlerSecurityService securityService;


    //用于判断是否使用lfs上传 1073741824 Bytes 1G
    private final long lfsFileMinSize=209715200;

    public static final String REGULAR_UPLOAD_TYPE = "regular";

    public static final String LFS_UPLOAD_TYPE = "lfs";

    protected RepositoryPathResolver repositoryPathResolver;

    public MlModelPreUploadDirLocalCommand(RepositoryPathResolver repositoryPathResolver) {
        this.repositoryPathResolver = repositoryPathResolver;
    }

    /**
     * pre 上传目录
     * @param context      上下文对象
     * @param filesRequest 上传文件请求
     */
    public MlFilesResponse preUploadDir(MlModelRequestContext context, MlFilesRequest filesRequest) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (filesRequest == null) {
            throw new NullPointerException("filesRequest is marked non-null but is null");
        }
        if (log.isDebugEnabled()) {
            log.debug("Pre-upload dir request received {}.", context);
        }
        if (filesRequest.getFiles() == null) {
            log.info("Files list is empty for {}.", context);
            throw new RuntimeException("Files list is empty");
        }
        assertValidNames(context);
        if (MlModelUtils.isReleaseRevision(context)) {
            assertModuleAlreadyExist(context);
        }
        List<MlFileInfo> fileInfos = new ArrayList<>();
        for (MlFile file : filesRequest.getFiles()) {
            if (log.isTraceEnabled()) {
                log.trace("File passed for pre-upload has path '{}' and size {} for context {}", file
                        .getPath(), Long.valueOf(file.getSize()), context);
            }
            if (file.getSize() > this.lfsFileMinSize) {
                fileInfos.add(new MlFileInfo(file.getPath(), "lfs", false));
                continue;
            }
            fileInfos.add(new MlFileInfo(file.getPath(), "regular", false));
        }
        return new MlFilesResponse(fileInfos);
    }

    /**
     * 验证模块是否已经存在
     * @param context 上下文对象
     */
    void assertModuleAlreadyExist(MlModelRequestContext context) {
        String repositoryId = context.getRepositoryId();
        String storageId = context.getStorageId();

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, MlModelUtils.getModelRevisionPath(context));
        if (!Files.exists(repositoryPath)) {
            return;
        }
        List<Path> fileList = new ArrayList<>();
        try {
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    // 在这里可以处理目录（如果需要的话）
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    if (!file.getFileName().toString().startsWith(".")
                            && !file.getFileName().toString().endsWith(".metadata")
                            && !file.getFileName().toString().endsWith(".md5")
                            && !file.getFileName().toString().endsWith(".sha1")
                            && !file.getFileName().toString().endsWith(".sha256")) {
                        fileList.add(file);
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
        String subRevisionPath = fileList.stream().findFirst().map(artifact -> artifact.getFileName().toString().replace(".folib_huggingface_model_info.json", "")).orElse(null);
        if (subRevisionPath != null) {
            String message = String.format("HuggingFace ML module conflict. Module: %s already exist in repoKey: %s.", subRevisionPath, repositoryId);
            log.info(message);
            throw new RuntimeException(message);
        }
    }

    /**
     * 验证名称
     * @param context 上下文对象
     */
    public void assertValidNames(MlModelRequestContext context) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        boolean isValidName = true;
        String uploadRejectionReason = "";
        String value = "";
        if (!MlModelUtils.isValidRevisionName(context.getRevision())) {
            isValidName = false;
            uploadRejectionReason = "revision name";
            value = context.getRevision();
        } else if (!MlModelUtils.isValidModelName(context.getModelName())) {
            isValidName = false;
            uploadRejectionReason = "model name";
            value = context.getModelName();
        } else if (!MlModelUtils.isValidOrganizationName(context.getOrg())) {
            isValidName = false;
            uploadRejectionReason = "Organization name";
            value = context.getOrg();
        }
        if (!isValidName) {
            String message = String.format("HuggingFace ML module upload rejected, due to invalid %s: %s in repoKey: %s.", uploadRejectionReason, value, context.getRepositoryId());
            log.info(message);
            throw new RuntimeException(message);
        }
    }
}

