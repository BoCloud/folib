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

import com.github.pagehelper.PageInfo;
import com.folib.entity.AccessToken;
import com.folib.forms.accesstoken.AccessTokenForm;
import com.folib.forms.accesstoken.AccessTokenResponse;
import com.folib.services.AccessTokenService;
import io.swagger.annotations.Api;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author veadan
 * @since 2024-08-20 11:17
 */
@RestController
@RequestMapping("/api/accessToken")
@Api(tags = "访问令牌")
@PreAuthorize("hasAuthority('ADMIN')")
public class AccessTokenController {

    @Resource
    private AccessTokenService accessTokenService;


    @PostMapping
    public ResponseEntity<AccessTokenResponse> generateToken(@RequestBody @Validated AccessTokenForm form) {
        AccessTokenResponse response = accessTokenService.generate(form);
        return ResponseEntity.ok(response);
    }

    @GetMapping
    public ResponseEntity<PageInfo<AccessToken>> list(Integer pageSize, Integer pageNum, @RequestParam(required = false) String tokenId) {
        List<AccessToken> tokens = accessTokenService.list(pageSize, pageNum, tokenId);
        return ResponseEntity.ok(PageInfo.of(tokens));
    }

    @DeleteMapping
    public ResponseEntity<String> delete(Long id,String tokenId){
        accessTokenService.delete(id,tokenId);
        return ResponseEntity.ok("revoked success");
    }


}
