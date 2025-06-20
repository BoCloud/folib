package com.veadan.folib.controllers.adapter.jfrog;

import com.veadan.folib.controllers.adapter.jfrog.constant.ResourcePermission;
import com.veadan.folib.controllers.adapter.jfrog.req.GroupReq;
import com.veadan.folib.controllers.adapter.jfrog.req.PermissionTargetReq;
import com.veadan.folib.controllers.adapter.jfrog.res.UserRes;
import com.veadan.folib.controllers.users.support.UserOutput;
import com.veadan.folib.controllers.users.support.UserResponseEntity;
import com.veadan.folib.converters.users.RoleConvert;
import com.veadan.folib.domain.User;
import com.veadan.folib.dto.RoleDTO;
import com.veadan.folib.dto.users.auth.*;
import com.veadan.folib.entity.UserGroup;
import com.veadan.folib.entity.UserGroupRef;
import com.veadan.folib.event.privilege.PrivilegeEventListenerRegistry;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.users.security.AuthoritiesProvider;
import com.veadan.folib.users.service.FolibRoleService;
import com.veadan.folib.users.service.UserGroupRefService;
import com.veadan.folib.users.service.UserGroupService;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
@RequestMapping("/artifactory/api/security")
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog用户权限", tags = "JFrog用户权限")
public class ArtifactSecurityController extends JFrogBaseController {


    private static final String STORAGE_NOT_FOUND_MESSAGE = "The storage was not found.";

    public static final String FAILED_CREATE_USER_GROUP = "无法创建用户组，因为提交的表单包含错误!";

    public static final String USER_GROUP_ALREADY_EXISTS = "用户组已存在";

    public static final String FAILED_CREATE_ROLE = "无法创建角色，因为提交的表单包含错误!";

    public static final String NOT_FOUND_USER = "指定的用户不存在!";

    @Inject
    private UserGroupService userGroupService;
    @Inject
    private UserGroupRefService userGroupRefService;
    @Inject
    private FolibRoleService folibRoleService;
    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;
    @Inject
    private AuthoritiesProvider authoritiesProvider;
    @Autowired
    private PrivilegeEventListenerRegistry privilegeEventListenerRegistry;


    @ApiOperation(value = "创建用户组")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PutMapping(value = {"/groups/{groupName}"})
    public ResponseEntity<?> createUserGroup(@PathVariable String groupName,
                                             @RequestBody GroupReq req,
                                             BindingResult bindingResult,
                                             @RequestHeader(HttpHeaders.ACCEPT) String accept) throws IOException {

        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_CREATE_USER_GROUP, bindingResult);
        }

        //todo :角色
        UserGroup userGroup = new UserGroup()
                .setGroupName(groupName)
                .setDescription(req.getDescription())
                .setJoinGroup(req.isAutoJoin() ? "1" : "0");
        List<UserGroup> userGroups = userGroupService.queryUserGroupList(userGroup);
        if (!userGroups.isEmpty()) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(getResponseEntityBody(USER_GROUP_ALREADY_EXISTS, accept));
        }
        userGroupService.save(userGroup);
        List<String> userIds = req.getUsersInGroup();
        if (userIds != null && !userIds.isEmpty()) {
            List<UserGroupRef> userGroupRefs = userIds.stream().map(userId -> {
                UserGroupRef userGroupRef = new UserGroupRef();
                userGroupRef.setUserId(userId);
                userGroupRef.setUserGroupName(userGroup.getGroupName());
                userGroupRef.setUserGroupId(userGroup.getId());
                return userGroupRef;
            }).collect(Collectors.toList());
            userGroupRefService.saveBath(userGroupRefs);
        }
        //同步用户组信息到其他节点
        privilegeEventListenerRegistry.dispatchUserGroupSyncEvent(String.valueOf(userGroup.getId()));
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @ApiOperation(value = "获取用户的详细信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping(value = {"/users/{userName}"})
    public ResponseEntity<?> getUserInfo(@PathVariable String userName) {
        User user = userService.findByUsername(userName);
        if (user == null) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(NOT_FOUND_USER);
        }

        UserOutput userOutput = UserOutput.fromUser(user);
        UserResponseEntity responseEntity = new UserResponseEntity(userOutput);
        responseEntity.setAssignableRoles(authoritiesProvider.getAssignableRoles());
        return ResponseEntity.ok(userResponseEntityToUserRes.apply(responseEntity));


    }

    @ApiOperation(value = "创建或替换权限目标")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PutMapping(value = {"/permissions/{permissionTargetName}"})
    public ResponseEntity<?> createOrReplacePermissionTarget(@PathVariable String permissionTargetName,
                                                             @RequestBody @Validated PermissionTargetReq req,
                                                             Authentication authentication) {

        if (!permissionTargetName.equals(req.getName())) {
            return ResponseEntity.status(HttpStatus.CONFLICT).body("The permission target name that was provided in the request path does not match the permission name in the provided permission configuration object.");
        }


        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        String username = loggedUser.getUsername();

        final String storageId = getDefaultStorageId(req.getRepositories().get(0));
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
        }

        List<String> groups = filterGroups(req.getPrincipals());
        if(groups != null && !groups.isEmpty()){
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("The group "+groups.toString()+" provided in the request parameter does not exist。");
        }

        List<String> users = filterUsers(req.getPrincipals());
        if(users != null && !users.isEmpty()){
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("The user "+users.toString()+" provided in the request parameter does not exist。");
        }

        RoleDto roleForm = reqToRoleForm.apply(req);

        RoleDTO roleDTO = RoleConvert.INSTANCE.formToDto(roleForm);
        if (roleDTO == null || roleDTO.getResources().isEmpty()) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(FAILED_CREATE_ROLE);
        }
        folibRoleService.save(roleDTO, username);
        //同步角色信息到其他节点
        privilegeEventListenerRegistry.dispatchRoleSyncEvent(roleDTO.getName());
        return ResponseEntity.status(HttpStatus.CREATED).body("");

    }

    public List<String> filterGroups(PermissionTargetReq.Principals principals){
        if(principals ==null || principals.getGroups().isEmpty()){
            return null;
        }
        List<String> groups = new ArrayList<>(principals.getGroups().keySet());
        List<String> list = userGroupService.findAll().stream().map(UserGroup::getGroupName).collect(Collectors.toList());
       return groups.stream().filter(group -> !list.contains(group)).collect(Collectors.toList());
    }

    public List<String> filterUsers(PermissionTargetReq.Principals principals){
        if(principals ==null || principals.getUsers().isEmpty()){
            return null;
        }
        List<String> users = new ArrayList<>(principals.getUsers().keySet());
        List<String> list = userService.getUsers().getUsers().stream().map(User::getUsername).collect(Collectors.toList());
        return users.stream().filter(user -> !list.contains(user)).collect(Collectors.toList());
    }

    Function<UserResponseEntity, UserRes> userResponseEntityToUserRes = userResponseEntity -> {
        return UserRes.builder()
                .name(userResponseEntity.getUser().getUsername())
                .email(userResponseEntity.getUser().getEmail())
                .admin(userResponseEntity.getUser().getRoles().stream().anyMatch(role -> role.equalsIgnoreCase("ADMIN")))
                .disableUIAccess(userResponseEntity.getUser().isEnabled())
                .profileUpdatable(true)
                .internalPasswordDisabled(true)
                .groups(new ArrayList<>(userResponseEntity.getUser().getUserGroups()))
                .watchManager(false)
                .policyManager(false)
                .build();
    };

    Function<Map<String, List<String>>, List<AccessUsers>> mapToAccessUsers = map -> {
        if (map.isEmpty()) {
            return null;
        }
        List<AccessUsers> list = new ArrayList<>();
        for (String key : map.keySet()) {
            AccessUsers accessUsers = AccessUsers.builder().id(key).access(map.get(key).stream()
                    .filter(d -> ResourcePermission.getByJPermission(d) != null)
                    .map(d -> {
                        return Objects.requireNonNull(ResourcePermission.getByJPermission(d)).getFPermission();
                    }).collect(Collectors.toList())).build();
            list.add(accessUsers);
        }
        return list;
    };

    Function<Map<String, List<String>>, List<AccessUserGroups>> mapToAccessUserGroups = map -> {
        if (map.isEmpty()) {
            return null;
        }
        List<AccessUserGroups> list = new ArrayList<>();
        Map<String, UserGroup> groupMap = userGroupService.findAll().stream().collect(Collectors.toMap(UserGroup::getGroupName, Function.identity()));
        for (String key : map.keySet()) {
            UserGroup group = groupMap.get(key);
            AccessUserGroups accessUserGroups = AccessUserGroups.builder()
                    .name(key)
                    .id(Long.toString(group.getId()))
                    .access(map.get(key).stream().filter(d -> ResourcePermission.getByJPermission(d) != null).map(d -> {
                        return Objects.requireNonNull(ResourcePermission.getByJPermission(d)).getFPermission();
                    }).collect(Collectors.toList())).build();
            list.add(accessUserGroups);
        }
        return list;
    };

    Function<PermissionTargetReq, RoleDto> reqToRoleForm = req -> {
        if (req == null) {
            return null;
        }
        List<AccessResources> resources = null;
        if (!req.getRepositories().isEmpty()) {
            final String storageId = getDefaultStorageId(req.getRepositories().get(0));
            resources = req.getRepositories().stream().map(repositoryId -> {
                AccessResources accessResources = new AccessResources();
                accessResources.setRepositoryId(repositoryId);
                accessResources.setStorageId(storageId);
                return accessResources;
            }).collect(Collectors.toList());
        }
        AccessModelDto privileges = null;
        if (req.getPrincipals() != null) {
            privileges = new AccessModelDto();
            privileges.setUsers(mapToAccessUsers.apply(req.getPrincipals().getUsers()));
            privileges.setGroups(mapToAccessUserGroups.apply(req.getPrincipals().getGroups()));

        }

        RoleDto roleForm = new RoleDto();
        roleForm.setResources(resources);
        roleForm.setName(req.getName());
        roleForm.setPrivileges(privileges);
        return roleForm;
    };


}
