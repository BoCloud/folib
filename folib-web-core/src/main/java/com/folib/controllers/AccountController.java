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

import javax.inject.Inject;

import com.folib.users.dto.UserDto;
import com.folib.components.auth.AuthComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.controllers.users.UserController;
import com.folib.controllers.users.support.UserOutput;
import com.folib.domain.UserRepositoryPermission;
import com.folib.forms.users.UserForm;
import com.folib.domain.User;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.Storage;
import com.folib.users.security.AuthoritiesProvider;
import com.folib.users.service.UserService;
import com.folib.users.service.impl.EncodedPasswordUser;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import com.folib.util.RSAUtils;
import com.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;
import java.util.Set;

/**
 * @author Steve Todorov
 */
@Controller
@RequestMapping("/api/account")
@Api(description = "账号管理",tags = "账号管理")
public class AccountController
        extends BaseController
{

    @Inject
    @Lazy
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    @Lazy
    private PasswordEncoder passwordEncoder;

    @Inject
    @Lazy
    private ConfigurationManager configurationManager;

    @Inject
    @Lazy
    private RSAUtils rsaUtils;
    @Autowired
    @Lazy
    private AuthoritiesProvider authoritiesProvider;

    @Autowired
    @Lazy
    private AuthComponent authComponent;

    @ApiOperation(value = "获取当前登录用户的帐户详细信息")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Returns account details"),
                            @ApiResponse(code = 403, message = "Unauthenticated access or user account has been disabled"),
                            @ApiResponse(code = 404, message = UserController.NOT_FOUND_USER) })
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @GetMapping(value = "",
                produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity getAccount(Authentication authentication)
    {

        if (!(authentication.getPrincipal() instanceof UserDetails))
        {
            String message = "Unsupported logged user principal type: " + authentication.getPrincipal().getClass();
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, MediaType.APPLICATION_JSON_VALUE);
        }

        User user = userService.findByUsername(authentication.getName());
        if (user == null)
        {
            return getNotFoundResponseEntity(UserController.NOT_FOUND_USER, MediaType.APPLICATION_JSON_VALUE);
        }

        return ResponseEntity.ok(UserOutput.fromUser(user));
    }

    @ApiOperation(value = "Get the account details of the currently logged user")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Account details have been successfully updated"),
                            @ApiResponse(code = 400, message = "Unsupported logged user principal type"),
                            @ApiResponse(code = 404, message = UserController.NOT_FOUND_USER) })
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @PutMapping(value = "",
                consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity updateAccount(@RequestBody @Validated(UserForm.UpdateAccount.class) UserForm userToUpdate,
                                        BindingResult bindingResult,
                                        Authentication authentication)
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(UserController.FAILED_UPDATE_USER, bindingResult);
        }
        // Updating account details currently only allows changing password and security token.
        // However, we're reusing the UserForm which includes other fields. Just to be on the safe side,
        // we are creating a new UserDto which contains only password & securityToken field changes.
        UserDto user = new UserDto();
        user.setUsername(userToUpdate.getUsername());
        if (StringUtils.isNotBlank(userToUpdate.getPassword())) {
            user.setOriginalPassword(userToUpdate.getPassword());
            String password = rsaUtils.decrypt(userToUpdate.getPassword());
            user.setPassword(password);
        }
        user.setEmail(userToUpdate.getEmail());
        user.setSecurityTokenKey(userToUpdate.getSecurityTokenKey());
        user.setAvatar(userToUpdate.getAvatar());
        userService.updateAccountDetailsByUsername(new EncodedPasswordUser(user, passwordEncoder));

        return getSuccessfulResponseEntity("Account details have been successfully updated",
                                           MediaType.APPLICATION_JSON_VALUE);
    }

    @ApiOperation(value = "获取当前用户对指定存储空间和仓库的权限信息")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Returns permissions details")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/permission/{storageId}/{repositoryId}",
            produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity<UserRepositoryPermission> getStorageAndRepositoryPermission(@ApiParam(value = "The storageId", required = true) @PathVariable String storageId, @ApiParam(value = "The repositoryId", required = true) @PathVariable String repositoryId) {
        Storage storage = configurationManager.getStorage(storageId);
        UserRepositoryPermission userRepositoryPermission = UserRepositoryPermission.builder().storageAdmin(storage.getAdmin()).permissions(Collections.emptySet()).build();
        userRepositoryPermission.setPermissions(authComponent.getAllPrivileges(storageId, repositoryId));
        return ResponseEntity.ok(userRepositoryPermission);
    }

    @ApiOperation(value = "获取当前用户对指定制品的权限信息")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Returns permissions details")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/permission/{storageId}/{repositoryId}/{artifactPath:.+}",
            produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity<Set<String>> getArtifactPermission(@ApiParam(value = "The storageId", required = true) @PathVariable String storageId, @ApiParam(value = "The repositoryId", required = true) @PathVariable String repositoryId, @ApiParam(value = "The artifactPath", required = true) @PathVariable String artifactPath) {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        return ResponseEntity.ok(authComponent.getPrivileges(repositoryPath));
    }
}
