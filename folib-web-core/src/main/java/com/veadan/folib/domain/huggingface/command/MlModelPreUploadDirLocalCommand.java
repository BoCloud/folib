package com.veadan.folib.domain.huggingface.command;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import javax.annotation.Nonnull;

import com.veadan.folib.domain.huggingface.constant.MlModelSystemProperties;
import com.veadan.folib.domain.huggingface.model.request.*;
import com.veadan.folib.domain.huggingface.utils.MlModelUtils;
import lombok.Generated;
import lombok.NonNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MlModelPreUploadDirLocalCommand {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(MlModelPreUploadDirLocalCommand.class);


    //private final PackageHandlerSecurityService securityService;


    //todo lfsFileMinSize
    private final long lfsFileMinSize=100000000;

    public static final String REGULAR_UPLOAD_TYPE = "regular";

    public static final String LFS_UPLOAD_TYPE = "lfs";

    public MlModelPreUploadDirLocalCommand() {

        //this.lfsFileMinSize = packageHandlerService.systemPropsService().getLongValue(MlModelSystemProperties.ML_MODEL_LFS_FILE_MIN_SIZE
        //        .name(), (Long) MlModelSystemProperties.ML_MODEL_LFS_FILE_MIN_SIZE.defaultValue()).longValue();
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
        //todo 待实现
        String repositoryId = context.getRepositoryId();
        //Stream<PackageArtifact> leadFiles = this.searchService.findArtifactsChildren(repoKey, MlModelUtils.getModelRevisionPath(context))
        //        .filter(artifact -> ".folib_huggingface_model_info.json".equals(artifact.getName()));
        //String subRevisionPath = leadFiles.findFirst().map(artifact -> artifact.getPath().replace(".folib_huggingface_model_info.json", "")).orElse(null);
        //if (subRevisionPath != null && !this.securityService.canDelete(repoKey, subRevisionPath)) {
        //    String message = String.format("HuggingFace ML module conflict. Module: %s already exist in repoKey: %s.", subRevisionPath, repoKey);
        //    log.info(message);
        //    throw new RuntimeException(message);
        //}
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

