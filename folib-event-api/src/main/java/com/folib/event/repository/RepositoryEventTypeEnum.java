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
package com.folib.event.repository;

/**
 * @author Veadan
 */
public enum RepositoryEventTypeEnum
{

    /**
     * Occurs when a repository has been created.
     */
    EVENT_REPOSITORY_CREATED(1),

    /**
     * Occurs when a repository has been deleted.
     */
    EVENT_REPOSITORY_DELETED(2),

    /**
     * Occurs when a repository has been put in service.
     */
    EVENT_REPOSITORY_PUT_IN_SERVICE(3),

    /**
     * Occurs when a repository has been put out of service.
     */
    EVENT_REPOSITORY_PUT_OUT_OF_SERVICE(4),

    /**
     * Occurs when a proxy repository's remote host has become unreachable.
     *
     * TODO: Not yet implemented.
     */
    EVENT_REPOSITORY_REMOTE_UNAVAILABLE(5),

    /**
     * Occurs when a proxy repository's remote host has become reachable.
     *
     * TODO: Not yet implemented.
     */
    EVENT_REPOSITORY_REMOTE_AVAILABLE(6),

    EVENT_REPOSITORY_DELETE_TO_CRON_JOB_DELETED(11),

    EVENT_REPOSITORY_DELETE_ALL_TO_CRON_JOB_DELETED(12);



    private int type;


    RepositoryEventTypeEnum(int type)
    {
        this.type = type;
    }

    public int getType()
    {
        return type;
    }

}
