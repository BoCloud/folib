package com.veadan.folib.controllers.users;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.users.support.UserGroupResponseEntity;
import com.veadan.folib.converters.users.UserGroupConvert;
import com.veadan.folib.dto.RoleResourceRefDTO;
import com.veadan.folib.dto.UserGroupDTO;
import com.veadan.folib.entity.UserGroup;
import com.veadan.folib.entity.UserGroupRef;
import com.veadan.folib.forms.users.UserForm;
import com.veadan.folib.forms.users.UserGroupForm;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.security.AuthoritiesProvider;
import com.veadan.folib.users.service.FolibRoleService;
import com.veadan.folib.users.service.UserGroupRefService;
import com.veadan.folib.users.service.UserGroupService;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import com.veadan.folib.util.RSAUtils;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
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

import javax.inject.Inject;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.veadan.folib.controllers.users.UserController.SUCCESSFUL_DELETE_USER;

/**
 * @author Veadan
 */
@Controller
@RequestMapping("/api/groups")
@Api(description = "用户组管理",tags = "用户组管理")
public class UserGroupController
        extends BaseController {

    public static final String SUCCESSFUL_CREATE_USER_GROUP = "用户组创建成功.";

    public static final String FAILED_CREATE_USER_GROUP = "无法创建用户组，因为提交的表单包含错误!";

    public static final String SUCCESSFUL_GET_USER = "已成功检索用户.";

    public static final String NOT_FOUND_USER_GROUP = "指定的用户组不存在!";

    public static final String SUCCESSFUL_GET_USERS = "已成功检索用户.";
    public static final String SUCCESSFUL_GET_USER_GROUP = "已成功检索用户组.";

    public static final String SUCCESSFUL_UPDATE_USER_GROUP = "用户组更新成功.";

    public static final String FAILED_UPDATE_USER_GROUP = "由于提交的表单包含错误，无法更新用户组!";

    public static final String SUCCESSFUL_DELETE_USER_GROUP = "该用户组已被删除.";

    public static final String FAILED_DELETE_USER_GROUP = "无法删除用户组.";

    public static final String OWN_USER_DELETE_FORBIDDEN = "无法删除自己";

    public static final String SUCCESSFUL_GENERATE_SECURITY_TOKEN = "安全令牌已生成.";

    public static final String FAILED_GENERATE_SECURITY_TOKEN = "无法生成 SecurityToken";

    public static final String SUCCESSFUL_UPDATE_ACCESS_MODEL = "自定义访问模型已更新";

    public static final String FAILED_UPDATE_ACCESS_MODEL = "无法更新访问模型.";

    public static final String USER_GROUP_DELETE_FORBIDDEN = "禁止删除此帐户组";

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private ConversionService conversionService;

    @Inject
    private AuthoritiesProvider authoritiesProvider;

    @Inject
    private PasswordEncoder passwordEncoder;

    @Inject
    private RSAUtils rsaUtils;
    @Inject
    private FolibRoleService folibRoleService;
    @Inject
    private StorageManagementService storageManagementService;
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
        if(!userIds.isEmpty()) {
            List<UserGroupRef> userGroupRefs = userIds.stream().map(userId -> {
                UserGroupRef userGroupRef = new UserGroupRef();
                userGroupRef.setUserId(userId);
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
        if(!roleIds.isEmpty()) {
            RoleResourceRefDTO resourceRefDTO = userGroupRefService.queryPrivilegeByGroup(groupId, "2", roleIds);
            responseEntity.setRoleResourceRefDTO(resourceRefDTO);
        }

        return ResponseEntity.ok(responseEntity);
    }
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_UPDATE_USER_GROUP),
            @ApiResponse(code = 400, message = FAILED_UPDATE_USER_GROUP)})
    @PreAuthorize("hasAuthority('UPDATE_USER')")
    @PutMapping(value = "{username}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity update(@ApiParam(value = "The name of the user", required = true)
                                 @PathVariable String username,
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
        if(!userIds.isEmpty()) {
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
}
