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

public final class MlModelConstants {
    public static final String X_REPO_COMMIT_HEADER = "X-Repo-Commit";

    public static final String X_ERROR_CODE_HEADER = "X-Error-Code";

    public static final String X_ERROR_MESSAGE_HEADER = "X-Error-Message";

    public static final String X_LINKED_ETAG = "X-Linked-ETag";

    public static final String X_LINKED_SIZE = "X-Linked-Size";

    public static final String ENTITY_NOT_FOUND_HEADER_VALUE = "EntryNotFound";

    public static final String CACHE_FOLDER = ".folib";

    public static final String UPLOADS_FOLDER = "_uploads";

    public static final String LAST_UPDATED_PROP_KEY = "hf_last_updated";

    public static final String LEAD_FILE_NAME = ".folib_huggingface_model_info.json";

    public static final String LATEST_LEAD_FILE_NAME = ".latest_huggingface_model_info.json";

    public static final String HF_ID = "huggingfaceml.id";

    public static final String HF_VERSION = "huggingfaceml.version";

    public static final String HF_GENERATED_REVISION_SHA1 = "huggingfaceml.generated.revision.sha1";

    public static final String HF_AUTHOR = "huggingfaceml.author";

    public static final String HF_LAST_MODIFIED = "huggingfaceml.lastModified";

    public static final String HF_LIBRARY_NAME = "huggingfaceml.libraryName";

    public static final String HF_TAGS = "huggingfaceml.tags";

    public static final String HF_LANGUAGE = "huggingfaceml.lang";

    public static final String HF_LICENSE = "huggingfaceml.license";

    public static final String HF_ETAG_FILE = "huggingfaceml.etag.file";

    public static final String ML_HANDLE_COMMIT_GUARD = "handle_commit_guard";


    private MlModelConstants() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }
}
