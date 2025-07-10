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
package com.folib.configuration;

public class ConfigurationUtils
{

    public static String getStorageId(String storageId,
                                      String storageAndRepositoryId)
    {
        String[] storageAndRepositoryIdTokens = storageAndRepositoryId.split(":");

        return storageAndRepositoryIdTokens.length == 2 ? storageAndRepositoryIdTokens[0] : storageId;
    }

    public static String getRepositoryId(String storageAndRepositoryId)
    {
        String[] storageAndRepositoryIdTokens = storageAndRepositoryId.split(":");

        return storageAndRepositoryIdTokens[storageAndRepositoryIdTokens.length < 2 ? 0 : 1];
    }

    public static String getPathStorageId(String storageAndRepositoryId)
    {
        String[] storageAndRepositoryIds = storageAndRepositoryId.split("/");

        return storageAndRepositoryIds[0];
    }

    public static String getPathRepositoryId(String storageAndRepositoryId)
    {
        String[] storageAndRepositoryIds = storageAndRepositoryId.split("/");

        return storageAndRepositoryIds[1];
    }

    public static String getPathTag(String storageAndRepositoryId)
    {
        String[] storageAndRepositoryIds = storageAndRepositoryId.split("/");

        return storageAndRepositoryIds[2];
    }

    public static String getStorageIdAndRepositoryId(String storageId, String repositoryId)
    {
       return String.format("%s:%s", storageId, repositoryId);
    }

    public static String getSpecialStorageIdAndRepositoryId(String storageId, String repositoryId)
    {
        return String.format("%s-%s", storageId, repositoryId);
    }
    
}
