package com.veadan.folib.constant;

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
    String ARTIFACT_NOT_FOUND_MESSAGE = "The artifact was not found.";

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
     * WAIT_LOCK_TIME
     */
    long WAIT_LOCK_TIME = 30L;

}
