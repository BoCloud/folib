package com.veadan.folib.controllers;

import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.users.UserController;
import com.veadan.folib.controllers.users.support.Permissions;
import com.veadan.folib.controllers.users.support.UserGroupResponseEntity;
import com.veadan.folib.controllers.users.support.UserOutput;
import com.veadan.folib.converters.users.RoleConvert;
import com.veadan.folib.converters.users.UserGroupConvert;
import com.veadan.folib.domain.PageResultResponse;
import com.veadan.folib.domain.User;
import com.veadan.folib.dto.*;
import com.veadan.folib.entity.FolibRole;
import com.veadan.folib.entity.UserGroup;
import com.veadan.folib.entity.UserGroupRef;
import com.veadan.folib.forms.users.auth.RoleForm;
import com.veadan.folib.forms.users.UserGroupForm;
import com.veadan.folib.scanner.common.msg.ObjectRestResponse;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.service.FolibRoleService;
import com.veadan.folib.users.service.RoleResourceRefService;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.veadan.folib.controllers.users.UserController.SUCCESSFUL_DELETE_USER;

/**
 * @author Fengmaogen
 */
@RestController
@RequestMapping("/api/auth")
@Api(value = "权限管理",tags = "权限管理")
public class RoleController extends BaseController {

    public static final String SUCCESSFUL_CREATE_ROLE = "角色创建成功.";

    public static final String FAILED_CREATE_ROLE = "无法创建角色，因为提交的表单包含错误!";

    public static final String SUCCESSFUL_GET_ROLE = "已成功检索角色.";

    public static final String NOT_FOUND_ROLE = "指定的角色不存在!";

    public static final String SUCCESSFUL_UPDATE_ROLE = "角色更新成功.";

    public static final String FAILED_UPDATE_ROLE = "由于提交的表单包含错误，无法更新角色!";

    public static final String SUCCESSFUL_DELETE_ROLE = "该角色已被删除.";
    public static final String DUPLICATE_ROLES = "该角色重复创建.";

    public static final String FAILED_DELETE_ROLE = "无法删除角色.";

    public static final String ROLE_DELETE_FORBIDDEN = "禁止删除此角色";
    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private RoleResourceRefService roleResourceRefService;
    @Inject
    private FolibRoleService folibRoleService;
    
    @ApiOperation(value = "获取用户的关联角色")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Returns account details"),
                            @ApiResponse(code = 403, message = "Unauthenticated access or user account has been disabled"),
                            @ApiResponse(code = 404, message = UserController.NOT_FOUND_USER) })
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @GetMapping(value = "/{userName}",
                produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity getAccount(@PathVariable String userName)
    {
        PageRequest pageRequest = PageRequest.of(0, 100);
        List<UserRoleDTO> rolesByUserName = roleResourceRefService.getRolesByUserName(userName, pageRequest);

        return ResponseEntity.ok(rolesByUserName);
    }

    @ApiOperation(value = "角色删除")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_DELETE_ROLE),
            @ApiResponse(code = 400, message = FAILED_DELETE_ROLE),
            @ApiResponse(code = 403, message = ROLE_DELETE_FORBIDDEN),
            @ApiResponse(code = 404, message = NOT_FOUND_ROLE)})
    @PreAuthorize("hasAuthority('DELETE_ROLE')")
    @DeleteMapping(value = "{roleId}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity delete(@ApiParam(value = "The name of the role") @PathVariable String roleId,
                                 Authentication authentication,
                                 @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (!(authentication.getPrincipal() instanceof UserDetails)) {
            String message = "Unsupported logged user principal type: " + authentication.getPrincipal().getClass();
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, accept);
        }
        FolibRole folibRole = folibRoleService.queryById(roleId);
        if (folibRole == null) {
            return getNotFoundResponseEntity(NOT_FOUND_ROLE, accept);
        }

        folibRoleService.deleteById(roleId);

        return getSuccessfulResponseEntity(SUCCESSFUL_DELETE_USER, accept);
    }

    @ApiOperation(value = "角色创建")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_CREATE_ROLE),
            @ApiResponse(code = 400, message = FAILED_CREATE_ROLE)})
    @PreAuthorize("hasAuthority('CREATE_ROLE')")
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity createGroup(@RequestBody @Validated(RoleForm.NewRole.class) RoleForm roleForm,
                                      BindingResult bindingResult,
                                      Authentication authentication,
                                      @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_CREATE_ROLE, bindingResult);
        }
        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        String username = loggedUser.getUsername();

        RoleDTO roleDTO = RoleConvert.INSTANCE.formToDto(roleForm);
        if(roleDTO == null) {
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, FAILED_CREATE_ROLE, accept);
        }
        folibRoleService.save(roleDTO, username);
        return getSuccessfulResponseEntity(SUCCESSFUL_CREATE_ROLE, accept);
    }

    @ApiOperation(value = "角色详情查询")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GET_ROLE),
            @ApiResponse(code = 404, message = NOT_FOUND_ROLE)})
    @PreAuthorize("hasAuthority('VIEW_ROLE')")
    @GetMapping(value = "{roleId}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity userGroupDetail(@PathVariable String roleId,
                                          @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        FolibRole folibRole = folibRoleService.queryById(roleId);
        if (folibRole == null) {
            return getNotFoundResponseEntity(NOT_FOUND_ROLE, accept);
        }
        RoleDTO roleDTO = roleResourceRefService.getUserByRoleId(roleId);
        //查角色关联的权限
        List<PermissionsDTO> permissions = roleResourceRefService.queryPermissions(roleId);
        List<AccessModelDTO.ApiAccess> apiAccess = permissions.stream()
                .filter(p -> GlobalConstants.RESOURCE_TYPE_PATH.equals(p.getResourceType()) && StringUtils.isNotEmpty(p.getApiAuthoritie()))
                .map(p -> AccessModelDTO.ApiAccess.builder().resourceId(p.getResourceId()).api(p.getApiAuthoritie()).build()).collect(Collectors.toList());

        Map<Long, List<PermissionsDTO>> permissionsMap = permissions.stream().filter(p -> !GlobalConstants.RESOURCE_TYPE_PATH.equals(p.getResourceType())).collect(Collectors.groupingBy(PermissionsDTO::getResourceId));
        List<RepositoryAccessModelDTO> repositoryAccessList = new ArrayList<>();
        permissionsMap.forEach((resourceId, permissionList) -> {
            List<String> privileges = permissionList.stream()
                    .flatMap(per -> Stream.of(
                            per.getPathPrivilege(),
                            per.getRepositoryPrivilege(),
                            per.getStoragePrivilege()
                    ))
                    .filter(StringUtils::isNotEmpty)
                    .collect(Collectors.toList());

            PermissionsDTO firstPermission = permissionList.get(0);
            RepositoryAccessModelDTO repositoryAccess = RepositoryAccessModelDTO.builder()
                    .resourceId(resourceId)
                    .repositoryId(firstPermission.getRepositoryId())
                    .storageId(firstPermission.getStorageId())
                    .path(firstPermission.getPath())
                    .privileges(privileges)
                    .build();

            repositoryAccessList.add(repositoryAccess);
        });
        roleDTO.setAccessModel(AccessModelDTO.builder().apiAccess(apiAccess).repositoriesAccess(repositoryAccessList).build());

        return ResponseEntity.ok(roleDTO);
    }
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_UPDATE_ROLE),
            @ApiResponse(code = 400, message = FAILED_UPDATE_ROLE)})
    @PreAuthorize("hasAuthority('UPDATE_ROLE')")
    @PutMapping(value = "{roleId}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity update(@ApiParam(value = "角色id必填", required = true)
                                 @PathVariable String roleId,
                                 @RequestBody @Validated RoleForm roleForm,
                                 Authentication authentication,
                                 BindingResult bindingResult,
                                 @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_UPDATE_ROLE, bindingResult);
        }
        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        String username = loggedUser.getUsername();

        RoleDTO roleDTO = RoleConvert.INSTANCE.formToDto(roleForm);
        if(roleDTO == null) {
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, FAILED_CREATE_ROLE, accept);
        }
        folibRoleService.updateRoleInfo(roleDTO, username);

        return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE_ROLE, accept);
    }

    @ApiOperation(value = "Used to retrieve users")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GET_ROLE)})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping(value = "/queryRole", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ObjectRestResponse<Page<FolibRole>> queryUser(@RequestBody String roleName, Integer page, Integer limit) {

        PageRequest pageRequest = PageRequest.of(page, limit);
        FolibRole folibRole = null;
        if(StringUtils.isNotEmpty(roleName)) {
            folibRole = FolibRole.builder().enName(roleName).build();

        }
        Page<FolibRole> folibRoles = folibRoleService.paginQuery(folibRole, pageRequest);
        if (Objects.isNull(folibRoles)) {
            return new ObjectRestResponse<>(false, null, "查询角色数据失败");
        }
        return ObjectRestResponse.ok(folibRoles);
    }


}
