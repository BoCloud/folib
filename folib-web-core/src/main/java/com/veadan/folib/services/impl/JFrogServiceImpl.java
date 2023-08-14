package com.veadan.folib.services.impl;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.URLUtil;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.enums.PromotionStatusEnum;
import com.veadan.folib.forms.externalnode.ExternalNodeForm;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.services.ExternalNodeService;
import com.veadan.folib.services.JFrogService;
import com.veadan.folib.util.RSAUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
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

import javax.inject.Inject;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class JFrogServiceImpl implements JFrogService {

    @Inject
    private ExternalNodeService externalNodeService;

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    private RSAUtils rsaUtils;

    @Override
    public Artifactory getArtifactory(String address, String username, String password) {
        return ArtifactoryClientBuilder.create()
                .setUrl(address)
                .setUsername(username)
                .setPassword(password)
                .setConnectionTimeout(10000)
                .build();
    }

    @Override
    public void validateArtifactory(String address, String username, String password) {
        listRepository(address, username, password);
    }

    @Override
    public List<LightweightRepository> listRepository(String address, String username, String password) {
        Artifactory artifactory = getArtifactory(address, username, password);
        return getLocalRepositoryList(artifactory);
    }

    @Override
    public List<LightweightRepository> listRepository(String address, String username, String password, String packageType) {
        Artifactory artifactory = getArtifactory(address, username, password);
        return getLocalRepositoryList(artifactory, packageType);
    }

    @Override
    public boolean existsRepository(String repositoryName, Artifactory artifactory) {
        List<LightweightRepository> localRepositoryList = getLocalRepositoryList(artifactory);
        return Optional.ofNullable(localRepositoryList).orElse(Collections.emptyList()).stream().anyMatch(repo -> repo.getKey().equals(repositoryName));
    }

    @Override
    public File uploadItem(String nodeName, String repositoryName, RepositoryPath repositoryPath, String artifactPath, Boolean recordStatus) {
        ExternalNodeForm externalNodeForm = getExternalNodeForm(nodeName);
        if (Objects.isNull(externalNodeForm)) {
            throw new BusinessException(String.format("制品库[%s]节点信息不存在", nodeName));
        }
        String address = externalNodeForm.getAddress(), username = externalNodeForm.getUsername(), password = externalNodeForm.getPassword();
        Artifactory artifactory = getArtifactory(address, username, password);
        try (InputStream pomInputStream = FileUtil.getInputStream(repositoryPath)) {
            artifactPath = URLUtil.encode(artifactPath);
            File file = artifactory.repository(repositoryName).upload(artifactPath, pomInputStream).bySha1Checksum(repositoryPath.getArtifactEntry().getChecksums().get(MessageDigestAlgorithms.SHA_1)).doUpload();
            if (Boolean.TRUE.equals(recordStatus)) {
                if (Objects.nonNull(file)) {
                    artifactComponent.handlerArtifactPromotion(nodeName, repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, PromotionStatusEnum.SUCCESS.getStatus());
                } else {
                    artifactComponent.handlerArtifactPromotion(nodeName, repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, PromotionStatusEnum.FAIL.getStatus());
                }
            }
            return file;
        } catch (Exception ex) {
            log.error("上传制品失败：{}", ExceptionUtils.getStackTrace(ex));
            log.info("存储空间：{} 仓库：{} 制品：{} 目标节点：{} 目标节点类型：{} 目标仓库：{} 目标路径：{} 上传结果：{}", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, externalNodeForm.getNodeName(), externalNodeForm.getType(), repositoryName, artifactPath, PromotionStatusEnum.FAIL.getStatus());
            getExceptionCode(ex);
            throw new BusinessException("制品上传失败:" + ex.getMessage());
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
            throw new BusinessException("制品库连接失败，请检查地址、账号、密码");
        }
        return localRepositoryList;
    }

    /**
     * 查询仓库列表
     *
     * @param artifactory artifactory
     * @param packageType 仓库类型
     * @return 仓库列表
     */
    private List<LightweightRepository> getLocalRepositoryList(Artifactory artifactory, String packageType) {
        List<LightweightRepository> localRepositoryList = getLocalRepositoryList(artifactory);
        return Optional.ofNullable(localRepositoryList).orElse(Collections.emptyList()).stream().filter(item -> item.getPackageType().equalsIgnoreCase(packageType)).collect(Collectors.toList());
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

    /**
     * 查询制品库节点信息
     *
     * @param nodeName 节点名称
     * @return 制品库节点信息
     */
    private ExternalNodeForm getExternalNodeForm(String nodeName) {
        ExternalNodeForm externalNode = externalNodeService.getExternalNode(ExternalNodeForm.builder().nodeName(nodeName).build());
        if (Objects.nonNull(externalNode)) {
            externalNode.setPassword(rsaUtils.decrypt(externalNode.getPassword()));
        }
        return externalNode;
    }
}
