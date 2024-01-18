package com.veadan.folib.constant;

import com.google.common.collect.Lists;

import java.util.List;

/**
 * @author leipenghui
 * @date 2022/11/29
 **/
public interface GlobalConstants {

    /**
     * 请求参数错误
     */
    String REQUEST_PARAMS_ERROR = "请求参数错误，请检查";

    /**
     * 存储空间未找到
     */
    String STORAGE_NOT_FOUND_MESSAGE = "The storage was not found.";

    /**
     * 仓库未找到
     */
    String REPOSITORY_NOT_FOUND_MESSAGE = "The repository was not found.";

    /**
     * 制品未找到
     */
    String ARTIFACT_NOT_FOUND_MESSAGE = "The artifact [%s-%s-%s] was not found.";

    /**
     * UI 访问前缀key
     */
    String WEB_URL_PREFIX = "WEB_URL_PREFIX";

    /**
     * 匿名用户
     */
    String ANONYMOUS_TOKEN_KEY = "anonymousUser";

    /**
     * 分布式锁名称
     */
    String DISTRIBUTED_LOCK_NAME = "distributedLock";

    /**
     * 分布式缓存名称
     */
    String DISTRIBUTED_CACHE_NAME = "distributedCache";

    /**
     * SHA_256
     */
    String SHA_256 = "sha256";

    /**
     * CHECKSUM_SHA_256
     */
    String CHECKSUM_SHA_256 = ".sha256";

    /**
     * SELF_METADATA
     */
    String SELF_METADATA = ".metadata";

    /**
     * FO_LIBRARY_METADATA
     */
    String FO_LIBRARY_METADATA = ".foLibrary-metadata";

    /**
     * NO_DATA
     */
    String NO_DATA = "NO_DATA";

    /**
     * WAIT_LOCK_TIME
     */
    long WAIT_LOCK_TIME = 30L;

    /**
     * LONG_WAIT_LOCK_TIME
     */
    long LONG_WAIT_LOCK_TIME = 120L;

    /**
     * point
     */
    String POINT = ".";

    /**
     * separator
     */
    String SEPARATOR = "/";

    /**
     * Docker层级信息目录名称列表
     */
    List<String> DOCKER_LAYER_DIR_NAME_LIST = Lists.newArrayList("blobs", "manifest");

    /**
     * maven后缀列表
     */
    List<String> MAVEN_EXTENSION_LIST = Lists.newArrayList(".jar", ".war", ".ear", ".pom");

}
