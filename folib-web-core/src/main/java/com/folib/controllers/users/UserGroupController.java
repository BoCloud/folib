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
import com.folib.constant.GlobalConstants;
import com.folib.controllers.BaseController;
import com.folib.controllers.users.support.UserGroupResponseEntity;
import com.folib.converters.users.UserGroupConvert;
import com.folib.dto.RoleResourceRefDTO;
import com.folib.dto.UserGroupDTO;
import com.folib.dto.UserGroupListDTO;
import com.folib.entity.UserGroup;
import com.folib.entity.UserGroupRef;
import com.folib.forms.users.UserGroupForm;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.users.service.UserGroupRefService;
import com.folib.users.service.UserGroupService;
import com.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.*;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;


/**
 * @author Veadan
 */
@Controller
@RequestMapping("/api/groups")
@Api(description = "用户组管理",tags = "用户组管理")
public class UserGroupController
        extends BaseController {

    public static final String SUCCESSFUL_DELETE_USER = "该用户已被删除.";
    public static final String SUCCESSFUL_GET_USER_GROUP = "用户组查询成功.";
    public static final String SUCCESSFUL_CREATE_USER_GROUP = "用户组创建成功.";

    public static final String FAILED_CREATE_USER_GROUP = "无法创建用户组，因为提交的表单包含错误!";

    public static final String SUCCESSFUL_GET_USER = "已成功检索用户.";

    public static final String NOT_FOUND_USER_GROUP = "指定的用户组不存在!";

    public static final String SUCCESSFUL_UPDATE_USER_GROUP = "用户组更新成功.";

    public static final String FAILED_UPDATE_USER_GROUP = "由于提交的表单包含错误，无法更新用户组!";

    public static final String SUCCESSFUL_DELETE_USER_GROUP = "该用户组已被删除.";

    public static final String FAILED_DELETE_USER_GROUP = "无法删除用户组.";

    public static final String USER_GROUP_DELETE_FORBIDDEN = "禁止删除此帐户组";

    @Inject
    private UserGroupService userGroupService;
    @Inject
    private UserGroupRefService userGroupRefService;

    @ApiOperation(value = "用户组删除")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_DELETE_USER_GROUP),
            @ApiResponse(code = 400, message = FAILED_DELETE_USER_GROUP),
            @ApiResponse(code = 403, message = USER_GROUP_DELETE_FORBIDDEN),
            @ApiResponse(code = 404, message = NOT_FOUND_USER_GROUP)})
    @PreAuthorize("hasAuthority('DELETE_USER_GROUP')")
    @DeleteMapping(value = "{groupId}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity delete(@ApiParam(value = "The name of the user group") @PathVariable Long groupId,
                                 @RequestBody @Validated(UserGroupForm.NewUserGroup.class) UserGroupForm userGroupForm,
                                 Authentication authentication,
                                 @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (!(authentication.getPrincipal() instanceof UserDetails)) {
            String message = "Unsupported logged user principal type: " + authentication.getPrincipal().getClass();
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, accept);
        }
        UserGroup userGroup = userGroupService.queryById(groupId);
        if (userGroup == null) {
            return getNotFoundResponseEntity(NOT_FOUND_USER_GROUP, accept);
        }

        userGroupService.deleteById(groupId);

        return getSuccessfulResponseEntity(SUCCESSFUL_DELETE_USER, accept);
    }

    @ApiOperation(value = "用户组创建")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_CREATE_USER_GROUP),
            @ApiResponse(code = 400, message = FAILED_CREATE_USER_GROUP)})
    @PreAuthorize("hasAuthority('CREATE_USER_GROUP')")
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity createGroup(@RequestBody @Validated(UserGroupForm.NewUserGroup.class) UserGroupForm userGroupForm,
                                      BindingResult bindingResult,
                                      @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_CREATE_USER_GROUP, bindingResult);
        }

        UserGroup userGroup = UserGroupConvert.INSTANCE.UserGroupToUserGroupForm(userGroupForm);

        if(userGroup == null) {
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, FAILED_CREATE_USER_GROUP, accept);
        }

        userGroupService.save(userGroup);
        List<String> userIds = userGroupForm.getUserIds();
        if(userIds != null && !userIds.isEmpty()) {
            List<UserGroupRef> userGroupRefs = userIds.stream().map(userId -> {
                UserGroupRef userGroupRef = new UserGroupRef();
                userGroupRef.setUserId(userId);
                userGroupRef.setUserGroupName(userGroup.getGroupName());
                userGroupRef.setUserGroupId(userGroup.getId());
                return userGroupRef;
            }).collect(Collectors.toList());

            userGroupRefService.saveBath(userGroupRefs);
        }

        return getSuccessfulResponseEntity(SUCCESSFUL_CREATE_USER_GROUP, accept);
    }



    @ApiOperation(value = "用户组详情查询")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GET_USER),
            @ApiResponse(code = 404, message = NOT_FOUND_USER_GROUP)})
    @PreAuthorize("hasAuthority('VIEW_USER_GROUP')")
    @GetMapping(value = "{groupId}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity userGroupDetail(@PathVariable Long groupId,
                                  @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        UserGroup userGroup = userGroupService.queryById(groupId);
        if (userGroup == null) {
            return getNotFoundResponseEntity(NOT_FOUND_USER_GROUP, accept);
        }
        //查询用户组关联的用户、角色
        UserGroupDTO userGroupDTO = userGroupService.queryGroupDetailById(groupId);
        UserGroupResponseEntity responseEntity = new UserGroupResponseEntity();
        responseEntity.setUserGroupDTO(userGroupDTO);
        //查询用户组关联的权限
        List<String> roleIds = userGroupDTO.getRoleIds();
        if(roleIds != null && !roleIds.isEmpty()) {
            List<RoleResourceRefDTO> resourceRefDTOs = userGroupRefService.queryPrivilegeByGroup(groupId, "2", roleIds);
            responseEntity.setRoleAccess(resourceRefDTOs.stream().collect(Collectors.toMap(RoleResourceRefDTO::getRoleId, roleResourceRefDTO ->
                    Stream.of(roleResourceRefDTO.getPathPrivileges(), roleResourceRefDTO.getRepositoryPrivileges(), roleResourceRefDTO.getStoragePrivileges()).filter(Objects::nonNull).flatMap(Collection::stream).collect(Collectors.toList()))));
        }

        return ResponseEntity.ok(responseEntity);
    }
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_UPDATE_USER_GROUP),
            @ApiResponse(code = 400, message = FAILED_UPDATE_USER_GROUP)})
    @PreAuthorize("hasAuthority('UPDATE_USER_GROUP')")
    @PutMapping(value = "{groupId}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity update(@ApiParam(value = "The name of the user", required = true)
                                 @PathVariable Long groupId,
                                 @RequestBody @Validated(UserGroupForm.ExistingUserGroup.class) UserGroupForm userGroupToUpdate,
                                 BindingResult bindingResult,
                                 @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_UPDATE_USER_GROUP, bindingResult);
        }

        UserGroup userGroup = UserGroupConvert.INSTANCE.UserGroupToUserGroupForm(userGroupToUpdate);

        if(userGroup == null) {
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, FAILED_CREATE_USER_GROUP, accept);
        }
        userGroupService.update(userGroup);
        List<String> userIds = userGroupToUpdate.getUserIds();
        if(userIds != null && !userIds.isEmpty()) {
            //删除原有关联用户
            userGroupRefService.deleteByUserGroupId(userGroup.getId());
            //维护关联用户
            List<UserGroupRef> userGroupRefs = userIds.stream().map(userId -> {
                UserGroupRef userGroupRef = new UserGroupRef();
                userGroupRef.setUserId(userId);
                userGroupRef.setUserGroupId(userGroup.getId());
                return userGroupRef;
            }).collect(Collectors.toList());

            userGroupRefService.saveBath(userGroupRefs);
        }

        return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE_USER_GROUP, accept);
    }

    @ApiOperation(value = "Used to retrieve users")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GET_USER_GROUP)})
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/queryUserGroup", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public TableResultResponse<UserGroupListDTO> queryUser(@RequestParam(name = "page", required = false) Integer page,
                                                           @RequestParam(name = "limit", required = false) Integer limit,
                                                           @RequestParam(name = "name", required = false) String name,
                                                           @RequestParam(name = "matchGroupName", required = false) String matchGroupName,
                                                           @RequestParam(name = "joinGroup", required = false) String joinGroup) {

        if (Objects.isNull(page) || page <1) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        PageRequest pageRequest = PageRequest.of(page, limit);
        UserGroup userGroup = UserGroup.builder().build();
        userGroup.setGroupName(name);
        userGroup.setMatchGroupName(matchGroupName);
        userGroup.setJoinGroup(joinGroup);
        userGroup.setDeleted(GlobalConstants.NOT_DELETED);
        PageInfo<UserGroupListDTO> userGroupListDTOS = userGroupService.pageQueryAndUserNumber(userGroup, pageRequest);
        if (Objects.isNull(userGroupListDTOS)) {
            return new TableResultResponse<>(0, null);
        }
        return new TableResultResponse<>(userGroupListDTOS.getTotal(), userGroupListDTOS.getList());

    }

}
