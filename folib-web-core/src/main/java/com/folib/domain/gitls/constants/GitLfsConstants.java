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
package com.folib.domain.gitls.constants;

public interface GitLfsConstants {
    public static final String LFS_API = "lfs";

    public static final String GITLFS_ENDPOINT = "gitlfs";

    public static final String OID = "OID";

    public static final String OBJECTS = "objects";

    public static final String LOCKS_DIR = ".jfrog/locks";

    public static final String PROP_LOCK_OWNER = "lock.owner";

    public static final String SHA_256_HEADER = "X-Checksum-Sha256";

    public static final String LFS_JSON = "application/vnd.git-lfs+json";

    public static final String BATCH_OPERATION_DOWNLOAD = "download";

    public static final String BATCH_OPERATION_UPLOAD = "upload";

    public static final String NOT_FOUND_MESSAGE = "Not Found";

    public static final String NO_DEPLOY_MESSAGE = "You do not have deploy permissions for the requested path";

    public static final String NO_READ_MESSAGE = "You do not have read permissions for the requested path";

    public static final String UNSUPPORTED_OPERATION = "Unsupported operation";

    public static final String PATH_SEPARATOR = "/";
}
