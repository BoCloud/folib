package com.folib.controllers.adapter.jfrog;

import cn.hutool.extra.spring.SpringUtil;
import com.google.common.collect.Maps;
import com.folib.components.DistributedCacheComponent;
import com.folib.controllers.BaseArtifactController;
import com.folib.storage.Storage;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

/**
 * @author veadan
 * @date 2023/10/12
 **/
public abstract class JFrogBaseController extends BaseArtifactController {

    private static final String REPOSITORY_NOT_FOUND_MESSAGE = "The %s repository was not found.";

    private static final String NOT_FOUND_MESSAGE = "The %s Artifact was not found.";

    /**
     * 获取设置默认的存储空间
     *
     * @param repositoryId 仓库名称
     * @return 存储空间
     */
    public String getDefaultStorageId(String repositoryId) {
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        if (StringUtils.isNotBlank(repositoryId)) {
            //按照仓库查询对应的存储空间
            String key = "JFrogAdapterStorage_" + repositoryId;
            String jFrogAdapterStorage = distributedCacheComponent.get(key);
            if (StringUtils.isNotBlank(jFrogAdapterStorage)) {
                return jFrogAdapterStorage;
            }
        }
        String key = "JFrogAdapterDefaultStorage";
        String jFrogAdapterDefaultStorage = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(jFrogAdapterDefaultStorage)) {
            throw new RuntimeException("Default storage not found,Please Set the default storageId");
        }
        return jFrogAdapterDefaultStorage;
    }

    /**
     * 校验存储空间和仓库是否存在
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @return 结果
     */
    public boolean checkRepository(String storageId, String repositoryId) {
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return false;
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return false;
        }
        return true;
    }

    /**
     * 仓库不存在
     *
     * @param type 源仓库 或者 目标仓库 可以为空
     */
    public ResponseEntity<Object> repositoryNotFound(String type) {
        if (StringUtils.isBlank(type)) {
            type = "";
        }
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, String.format(REPOSITORY_NOT_FOUND_MESSAGE, type)));
    }

    /**
     * 仓库不存在
     */
    public ResponseEntity<Object> repositoryNotFound() {
        return repositoryNotFound("");
    }

    /**
     * 制品不存在
     *
     * @param artifact 制品
     */
    public ResponseEntity<Object> artifactNotFound(String artifact) {
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, String.format(NOT_FOUND_MESSAGE, artifact)));
    }

    /**
     * 制品不存在
     */
    public ResponseEntity<Object> artifactNotFound() {
        return artifactNotFound("");
    }

    /**
     * 处理错误
     *
     * @param status  状态
     * @param message 消息
     * @return 结果
     */
    public Map<String, Object> handlerErrors(Integer status, String message) {
        Map<String, Object> result = Maps.newHashMap();
        Map<String, Object> resultData = Maps.newHashMap();
        if (Objects.isNull(status)) {
            status = HttpStatus.NOT_FOUND.value();
        }
        resultData.put("status", status);
        if (StringUtils.isBlank(message)) {
            message = NOT_FOUND_MESSAGE;
        }
        resultData.put("message", message);
        result.put("errors", Collections.singletonList(resultData));
        return result;
    }
}
