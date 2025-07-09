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
package com.folib.controllers;

import com.folib.constant.GlobalConstants;
import io.swagger.annotations.Api;
import org.apache.commons.lang.StringUtils;
import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

/**
 * @author Steve Todorov
 */
@Controller
@Api(description = "跳转首页/错误页控制", tags = "跳转首页/错误页控制")
public class UiController implements ErrorController {

    //@GetMapping(path = {"/robots.txt"})
    //public ResponseEntity robots(HttpServletResponse response) {
    //    return ResponseEntity.status(HttpStatus.NOT_FOUND.value()).build();
    //}

    //@GetMapping(path = {"/**"}, produces = {MediaType.TEXT_HTML_VALUE})
    //public String indexWithRoute(HttpServletRequest request, HttpServletResponse response) {
    //    String path = request.getRequestURI();
    //    if (path.startsWith("/error")) {
    //        // 适配webdav 能够401返回给客户端而不发生重定向
    //        return null;
    //    }
    //    response.setStatus(HttpStatus.NOT_FOUND.value());
    //    return String.format("redirect:%s",getUIIndex());
    //}

    @GetMapping(path = {"/"}, produces = {MediaType.TEXT_HTML_VALUE})
    public String index() {

        return String.format("redirect:%s",getUIIndex());
    }


    public String getErrorPath() {
        return "/error";
    }

    private String getUIIndex() {
        String webUrlPrefix = System.getProperty(GlobalConstants.WEB_URL_PREFIX);
        if (StringUtils.isBlank(webUrlPrefix)) {
            webUrlPrefix = "/ui/";
        }
        return webUrlPrefix + "index.html";
    }



}
