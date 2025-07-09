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
package com.folib.controllers.restart;

import com.folib.app.FolibSpringBootApplication;
import com.folib.controllers.BaseController;
import io.swagger.annotations.Api;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import static com.folib.controllers.restart.RestartController.MAPPING;


/**
 * @author: adavid9
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping(value = MAPPING)
@Api(description = "服务重启控制器",tags = "服务重启控制器")
public class RestartController
        extends BaseController
{

    public static final String MAPPING = "/api";

    private static final Logger logger = LoggerFactory.getLogger(RestartController.class);

    @PostMapping("/restart")
    public void restart()
    {
        FolibSpringBootApplication.restart();
        logger.info("Restarting folib application.");
    }
}
