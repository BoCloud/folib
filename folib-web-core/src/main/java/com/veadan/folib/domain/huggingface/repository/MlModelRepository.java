package com.veadan.folib.domain.huggingface.repository;

import java.io.InputStream;
import javax.ws.rs.core.Response;

import com.veadan.folib.domain.gitls.model.GitLfsBatchJson;
import com.veadan.folib.domain.huggingface.model.RevisionData;
import com.veadan.folib.domain.huggingface.model.request.MlFilesRequest;
import com.veadan.folib.domain.huggingface.model.request.MlFilesResponse;
import com.veadan.folib.domain.huggingface.model.request.MlModelRequestContext;
import lombok.NonNull;
import org.springframework.http.ResponseEntity;


public interface MlModelRepository {

    /**
     * 获取请求头
     * @param paramMlModelRequestContext 请求上下文
     * @return
     */
    ResponseEntity<?> fetchHeaders( MlModelRequestContext paramMlModelRequestContext);

    /**
     * 上传文件
     * @param paramMlModelRequestContext 请求上下文
     * @param paramMlFilesRequest 上传文件请求
     * @return
     */
    MlFilesResponse handlePreUpload( MlModelRequestContext paramMlModelRequestContext,  MlFilesRequest paramMlFilesRequest);

    /**
     * 处理 Lfs 预上传
     * @param paramMlModelRequestContext 请求上下文
     * @param paramGitLfsBatchJson git lfs 批量上传json
     * @return GitLfsBatchJson
     */
    GitLfsBatchJson handleLfsPreUpload( MlModelRequestContext paramMlModelRequestContext,  GitLfsBatchJson paramGitLfsBatchJson);

    /**
     *  处理提交
     * @param paramMlModelRequestContext 请求上下文
     * @param paramInputStream 输入流
     * @return
     */
    String handleCommit( MlModelRequestContext paramMlModelRequestContext,  InputStream paramInputStream);

    /**
     * 获取文件
     * @param paramMlModelRequestContext 请求上下文
     * @return
     */
    ResponseEntity<?> fetchFile( MlModelRequestContext paramMlModelRequestContext);

    /**
     * 上传 Lfs 文件
     * @param paramMlModelRequestContext 请求上下文
     * @param paramInputStream 输入流
     * @return
     */
    ResponseEntity<?> uploadLfsFile( MlModelRequestContext paramMlModelRequestContext,  InputStream paramInputStream);

    /**
     * 获取版本信息
     * @param paramMlModelRequestContext 请求上下文
     * @return
     */
    RevisionData fetchRevisionData( MlModelRequestContext paramMlModelRequestContext);

}
