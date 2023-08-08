package com.veadan.folib.services.impl;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.URLUtil;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.services.JFrogService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.client.HttpResponseException;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;
import org.jfrog.artifactory.client.impl.CopyMoveException;
import org.jfrog.artifactory.client.model.CopyMoveResultMessage;
import org.jfrog.artifactory.client.model.CopyMoveResultReport;
import org.jfrog.artifactory.client.model.File;
import org.jfrog.artifactory.client.model.LightweightRepository;
import org.jfrog.artifactory.client.model.impl.RepositoryTypeImpl;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import java.io.InputStream;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class JFrogServiceImpl implements JFrogService {

    @Override
    public Artifactory getArtifactory(String address, String userName, String password) {
        return ArtifactoryClientBuilder.create()
                .setUrl(address)
                .setUsername(userName)
                .setPassword(password)
                .setConnectionTimeout(10000)
                .build();
    }

    @Override
    public void validateArtifactory(String address, String userName, String password) {
        listRepository(address, userName, password);
    }

    @Override
    public List<LightweightRepository> listRepository(String address, String userName, String password) {
        Artifactory artifactory = getArtifactory(address, userName, password);
        return getLocalRepositoryList(artifactory);
    }

    @Override
    public boolean existsRepository(String repositoryName, Artifactory artifactory) {
        List<LightweightRepository> localRepositoryList = getLocalRepositoryList(artifactory);
        return Optional.ofNullable(localRepositoryList).orElse(Collections.emptyList()).stream().anyMatch(repo -> repo.getKey().equals(repositoryName));
    }

    @Override
    public File uploadItem(Object param, java.io.File file, String path) {
        String address = "", username = "", password = "", repositoryName = "";
        Artifactory artifactory = getArtifactory(address, username, password);
        try (InputStream pomInputStream = FileUtil.getInputStream(file)) {
            path = URLUtil.encode(path);
            return artifactory.repository(repositoryName).upload(path, pomInputStream).doUpload();
        } catch (Exception ex) {
            log.error("上传制品失败：{}", ExceptionUtils.getStackTrace(ex));
            getExceptionCode(ex);
            throw new BusinessException("制品上传失败:" + ex.getMessage());
        } finally {
            //删除临时文件
            FileUtil.del(file);
        }
    }

    /**
     * 查询仓库列表
     *
     * @param artifactory artifactory
     * @return 仓库列表
     */
    private List<LightweightRepository> getLocalRepositoryList(Artifactory artifactory) {
        List<LightweightRepository> localRepositoryList;
        try {
            localRepositoryList = artifactory.repositories().list(RepositoryTypeImpl.LOCAL);
        } catch (Exception ex) {
            log.error("查询仓库列表错误：{}", ExceptionUtils.getStackTrace(ex));
            getExceptionCode(ex);
            throw new BusinessException("制品库连接失败，请检查账号密码");
        }
        return localRepositoryList;
    }

    /**
     * 获取错误信息
     *
     * @param e 异常
     */
    public void getExceptionCode(Exception e) {
        if (e instanceof HttpResponseException) {
            int code = ((HttpResponseException) e).getStatusCode();
            if (code == HttpStatus.UNAUTHORIZED.value()) {
                throw new BusinessException("操作失败：身份信息验证失败");
            }
            if (code == HttpStatus.FORBIDDEN.value()) {
                throw new BusinessException("操作失败：权限不足，操作被禁止");
            }
            if (code == HttpStatus.NOT_FOUND.value()) {
                throw new BusinessException("操作失败：jfrog仓库不存在或已被删除");
            }
        }
        if (e instanceof CopyMoveException) {
            CopyMoveResultReport copyMoveResultReport = ((CopyMoveException) e).getCopyMoveResultReport();
            List<CopyMoveResultMessage> messages = copyMoveResultReport.getMessages();
            if (CollectionUtil.isEmpty(messages)) {
                return;
            }
            StringBuilder message = new StringBuilder();
            messages.forEach(msg -> message.append(msg.getMessage()));
            String permissions = "permissions";
            if (message.indexOf(permissions) >= 0) {
                throw new BusinessException("操作失败:权限不足，操作被禁止");
            }
        }
    }
}
