package com.veadan.folib.domain.huggingface.repository;

import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.gitls.model.GitLfsBatchJson;
import com.veadan.folib.domain.huggingface.command.*;
import com.veadan.folib.domain.huggingface.model.RevisionData;
import com.veadan.folib.domain.huggingface.model.request.MlFilesRequest;
import com.veadan.folib.domain.huggingface.model.request.MlFilesResponse;
import com.veadan.folib.domain.huggingface.model.request.MlModelRequestContext;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.HuggingFaceLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactManagementService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import javax.servlet.http.HttpServletResponse;
import java.io.InputStream;

@Service
public class MlModelLocalRepository implements MlModelRepository {

    private static final Logger log = LoggerFactory.getLogger(MlModelLocalRepository.class);

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    protected ArtifactRepository artifactRepository;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactManagementService artifactManagementService;
    @Inject
    private DistributedLockComponent distributedLockComponent;
    @Inject
    private HuggingFaceLayoutProvider layoutProvider;
    /**
     * 获取请求头
     * @param context 请求上下文
     * @return
     */
    @Override
    public ResponseEntity<?> fetchHeaders(MlModelRequestContext context, HttpServletResponse response) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        RevisionData revisionData = fetchRevisionData(context);
        return new MlModelFetchFileOrHeadersLocalCommand().fetchFile(context, revisionData);
    }

    /**
     * 上传文件
     * @param context      请求上下文
     * @param filesRequest 上传文件请求
     * @return
     */
    @Override
    public MlFilesResponse handlePreUpload(MlModelRequestContext context, MlFilesRequest filesRequest) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (filesRequest == null) {
            throw new NullPointerException("filesRequest is marked non-null but is null");
        }
        return (new MlModelPreUploadDirLocalCommand(repositoryPathResolver)).preUploadDir(context, filesRequest);
    }

    /**
     * 处理 Lfs 预上传
     *
     * @param context 请求上下文
     * @param lfsInfoPayload       git lfs 批量上传json
     * @return GitLfsBatchJson
     */
    @Override
    public GitLfsBatchJson handleLfsPreUpload(MlModelRequestContext context, GitLfsBatchJson lfsInfoPayload) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (lfsInfoPayload == null) {
            throw new NullPointerException("lfsInfoPayload is marked non-null but is null");
        }

        MlModelLfsPreUploadCommand preUploadCommand = new MlModelLfsPreUploadCommand(artifactRepository,configurationManager,repositoryPathResolver,artifactManagementService);
        return preUploadCommand.preUploadBatch(context.getStorageId(),context.getRepositoryId(), context.getOrg(), context.getModelName(), lfsInfoPayload, context.getRequest());
    }

    /**
     * 处理提交
     * @param context 请求上下文
     * @param bodyStream  输入流
     * @return
     */
    @Override
    public String handleCommit(MlModelRequestContext context, InputStream bodyStream) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (bodyStream == null) {
            throw new NullPointerException("bodyStream is marked non-null but is null");
        }
        return (new MlModelUploadDirLocalCommand(repositoryPathResolver, artifactManagementService, layoutProvider,artifactRepository,distributedLockComponent)).uploadDir(context, bodyStream);
    }

    /**
     * 获取文件
     * @param context 请求上下文
     * @return
     */
    @Override
    public ResponseEntity<?> fetchFile(MlModelRequestContext context) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        RevisionData revisionData = fetchRevisionData(context);
        return (new MlModelFetchFileOrHeadersLocalCommand()).fetchFile(context, revisionData);
    }

    /**
     * 上传 Lfs 文件
     * @param context 请求上下文
     * @param stream 文件流
     * @return
     */
    @Override
    public ResponseEntity<?> uploadLfsFile(MlModelRequestContext context, InputStream stream) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (stream == null) {
            throw new NullPointerException("stream is marked non-null but is null");
        }
        return (new MlModelUploadLfsFilesCommand(repositoryPathResolver, artifactManagementService,artifactRepository)).uploadFile(context, stream);
    }

    /**
     * 获取版本信息
     * @param context 请求上下文
     * @return
     */
    @Override
    public RevisionData fetchRevisionData(MlModelRequestContext context) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        try {
            return new MlModelFetchRevisionLocalCommand(repositoryPathResolver).fetchRevision(context);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
}
