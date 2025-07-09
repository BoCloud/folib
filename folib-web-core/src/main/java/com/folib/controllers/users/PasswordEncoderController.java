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
package com.folib.controllers.users;

import com.folib.forms.users.PasswordEncodeForm;
import com.folib.controllers.BaseController;
import com.folib.validation.RequestBodyValidationException;

import javax.inject.Inject;
import java.util.Collections;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * @author veadan
 */
@Controller
@RequestMapping("/api/users/encrypt/password")
@Api(description = "用户密码加密管理",tags = "用户密码加密管理")
@PreAuthorize("hasAuthority('ADMIN')")
public class PasswordEncoderController
        extends BaseController
{

    public static final String INVALID_FORM = "Form contains invalid data!";

    @Inject
    private PasswordEncoder passwordEncoder;

    @ApiOperation(value = "Encodes submitted raw password")
    @ApiResponses(value = @ApiResponse(code = 200, message = "Returns encoded password"))
    @PostMapping(consumes = { MediaType.APPLICATION_JSON_VALUE },
                 produces = { MediaType.APPLICATION_JSON_VALUE,
                              MediaType.TEXT_PLAIN_VALUE })
    @ResponseBody
    public ResponseEntity encode(@RequestHeader(HttpHeaders.ACCEPT) String accept,
                                 @Validated @RequestBody PasswordEncodeForm form,
                                 BindingResult bindingResult)
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(INVALID_FORM, bindingResult);
        }

        String encoded = passwordEncoder.encode(form.getPassword());
        Object response = encoded;

        if (accept.equals(MediaType.APPLICATION_JSON_VALUE))
        {
            response = Collections.singletonMap("password", encoded);
        }

        return ResponseEntity.ok().body(response);
    }

}
