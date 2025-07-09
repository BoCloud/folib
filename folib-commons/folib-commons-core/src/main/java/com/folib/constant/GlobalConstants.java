/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.constant;

import com.google.common.collect.Lists;

import java.util.List;

/**
 * @author veadan
 * @date 2022/11/29
 **/
public interface GlobalConstants {

    /**
     * 请求参数错误
     */
    String REQUEST_PARAMS_ERROR = "请求参数错误，请检查";

    /**
     * blob未找到
     */
    String DOCKER_BLOB_NOT_FOUND_MESSAGE = "The blob [%s] was not found.";

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
     * DEFAULT_REFRESH_CONTENT_INTERVAL
     */
    int DEFAULT_REFRESH_CONTENT_INTERVAL = 60;

    /**
     * WAIT_LOCK_TIME
     */
    long WAIT_LOCK_TIME = 60L;

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

    /**
     * docker v2
     */
    String DOCKER_V2 = "/v2";

    /**
     * library
     */
    String DOCKER_DEFAULT_REPO = "library";

    /**
     * docker level single
     */
    String DOCKER_LEVEL_SINGLE = "single";

    /**
     * latest
     */
    String LATEST = "latest";

    /**
     * asterisk
     */
    String ASTERISK = "*";

    /**
     * download
     */
    String DOWNLOAD = "download";

    /**
     * drop
     */
    String DROP = "drop";

    /**
     * ws节点key
     */
    String WS_NODE_KEY = "WS_NODE_KEY";

    /**
     * NotFound
     */
    String NOT_FOUND = "NotFound";

    /**
     * @
     */
    String AT = "@";

    /**
     * 删除
     */
    String DELETED = "1";
    /**
     * 未删除
     */
    String NOT_DELETED = "0";

    /**
     * 默认
     */
    String DEFALUT = "1";
    /**
     * 非默认
     */
    String NOT_DEFAULT = "0";
    /**
     * 角色关联类型-用户
     */
    String ROLE_TYPE_USER = "1";
    /**
     * 角色关联类型-用户组
     */
    String ROLE_TYPE_USER_GROUP = "2";
    /**
     * 资源类型-api
     */
    String RESOURCE_TYPE_API = "1";
    /**
     * 资源类型-存储空间
     */
    String RESOURCE_TYPE_STORAGE = "2";
    /**
     * 资源类型-仓库
     */
    String RESOURCE_TYPE_REPOSITORY = "3";
    /**
     * 资源类型-路径
     */
    String RESOURCE_TYPE_PATH = "4";

    /**
     * 用户启用
     */
    String USER_ENABLE = "true";
    /**
     * 用户禁用
     */
    String USER_NOT_ENABL = "false";

    /**
     * 扫描重试
     */
    String SCAN_RETRY = "ScanRetryCount";

    /**
     * 扫描重试
     */
    String SCAN_RETRY_KEY = "SCAN_RETRY";

    /**
     * 扫描重试次数
     */
    String SCAN_RETRY_COUNT_KEY = "SCAN_RETRY_COUNT";

    /**
     * 扫描重试次数
     */
    Integer SCAN_RETRY_COUNT = 3;

    /**
     * 扫描单文件最大限制
     */
    String SCAN_MAX_SIZE_KEY = "SCAN_MAX_SIZE";

    /**
     * 扫描单文件最大限制GB
     */
    Integer SCAN_MAX_SIZE = 3;

    /**
     * DEFAULT_CONTENT_TIME
     */
    int DEFAULT_CONTENT_TIME = 10;

    /**
     * DEFAULT_READ_TIME
     */
    int DEFAULT_READ_TIME = 60;

    /**
     * 逗号
     */
    String COMMA = ",";

    /**
     * 扫描仓库key
     */
    String SCAN_ENABLE_REPOSITORY_KEY = "SCAN_ENABLE_REPOSITORY_KEY_%s";
    /**
     * 制品晋级并发线程数量
     */
    int FOLIB_PROMOTION_THREAD = 4;

    /**
     * 制品晋级并发线程数key
     */
    String FOLIB_PROMOTION_THREAD_KEY = "FOLIB_PROMOTION_THREAD";
    /**
     * websocket响应超时时间 秒单位
     */
    int WS_REQUEST_TIMOUT = 5;
    /**
     * websocket响应超时时间key
     */
    String WS_REQUEST_TIMOUT_KEY = "WS_REQUEST_TIMOUT";

    /**
     * websocket发送超时时间单位秒
     */
    int WS_SEND_TIMEOUT = 3;

    /**
     * websocket发送超时时间key
     */
    String WS_SEND_TIMEOUT_KEY = "WS_SEND_TIMEOUT";

    /**
     * 制品晋级websocket请求超时时间 单位秒
     */
    int WS_REQUEST_TIMOUT_OF_ARTIFACT_UPLOAD = 600;
    /**
     * 制品晋级websocket请求超时时间key
     */
    String WS_REQUEST_TIMOUT_OF_ARTIFACT_UPLOAD_KEY = "WS_REQUEST_TIMOUT_OF_ARTIFACT_UPLOAD";

    /**
     * 仓库路径查询线程数
     */
    int REPOSITORY_PATH_THREAD = 16;
    /**
     * 仓库路径查询线程数key
     */
    String REPOSITORY_PATH_THREAD_KEY = "REPOSITORY_PATH_THREAD";
    /**
     * 仓库路径查询批量大小
     */
    int REPOSITORY_PATH_BATCH_SIZE = 40;
    /**
     * 仓库路径查询批量大小key
     */
    String REPOSITORY_PATH_BATCH_SIZE_KEY = "REPOSITORY_PATH_BATCH_SIZE";
    /**
     * 元数据级别制品生命周期
     */
    String ARTIFACT_LIFE_CYCLE_KEY = "ARTIFACT_LIFE_CYCLE";
    /**
     * 元数据级别制品生命周期永久保留
     */
    String ARTIFACT_RETENTION_FOREVER_KEY = "forever";
    /**
     * 可忽略的线程前缀
     */
    List<String> IGNORE_THREAD_NAME_LIST = Lists.newArrayList("asyncWsCommand", "cron-task-pool-", "scheduled-pool-", "async", "boundedElastic", "custom");

    /**
     * 0
     */
    Integer ZERO = 0;

    /**
     * 最新版本关键字
     */
    List<String> LATEST_ARTIFACT_KEY_LIST = Lists.newArrayList("[RELEASE]", "[LATEST]");

    List<String> HTTP_PREFIX_LIST = Lists.newArrayList("http://", "https://");

    String COLON = ":";

    String RELEASE_ARTIFACT_KEY ="[RELEASE]";

    /**
     * 桶容量
     */
    long BUCKET_CAPACITY =3000;
    /**
     * 桶生成令牌量
     */
    long BUCKET_TOKENS=3000;

    String BUCKET_CAPACITY_KEY="BUCKET_CAPACITY";

    String BUCKET_TOKENS_KEY="BUCKET_TOKENS";
}
