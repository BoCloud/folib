package com.veadan.folib.controllers;

import cn.hutool.core.date.DateUtil;
import com.github.pagehelper.PageInfo;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.users.UserController;
import com.veadan.folib.converters.users.RoleConvert;
import com.veadan.folib.converters.users.UserGroupConvert;
import com.veadan.folib.converts.UserConvert;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.PrivilegeDispatch;
import com.veadan.folib.dto.*;
import com.veadan.folib.entity.*;
import com.veadan.folib.enums.SyncStrategyEnum;
import com.veadan.folib.event.privilege.PrivilegeEventListenerRegistry;
import com.veadan.folib.event.privilege.PrivilegeEventTypeEnum;
import com.veadan.folib.forms.users.auth.RoleForm;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.dto.UserAuthDTO;
import com.veadan.folib.users.service.*;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import com.veadan.folib.validation.RequestBodyValidationException;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import com.veadan.folib.ws.server.Command;
import com.veadan.folib.ws.server.WSMessageRequest;
import com.veadan.folib.ws.server.WSMessageResponse;
import io.swagger.annotations.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
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
import javax.servlet.http.HttpServletRequest;
import javax.websocket.Session;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Fengmaogen
 */
@Slf4j
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
    @Autowired
    private FolibUserService folibUserService;

    @Autowired
    private UserGroupService userGroupService;
    @Autowired
    private UserGroupRefService userGroupRefService;

    @Autowired
    private ResourceService resourceService;
    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;
    @Autowired
    private PrivilegeEventListenerRegistry privilegeEventListenerRegistry;

    @ApiOperation(value = "获取用户的关联角色")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Returns account details"),
                            @ApiResponse(code = 403, message = "Unauthenticated access or user account has been disabled"),
                            @ApiResponse(code = 404, message = UserController.NOT_FOUND_USER) })
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @GetMapping(value = "/user/{userName}",
                produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity getAccount(@PathVariable String userName)
    {
        PageRequest pageRequest = PageRequest.of(0, 100);
        List<UserRoleDTO> rolesByUserName = roleResourceRefService.getRolesByUserName(userName);

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
        if (SystemRole.ADMIN.name().equalsIgnoreCase(roleId)) {
            throw new RuntimeException("Cannot delete the admin role");
        }
        FolibRole folibRole = folibRoleService.queryById(roleId);
        if (folibRole == null) {
            return getNotFoundResponseEntity(NOT_FOUND_ROLE, accept);
        }

        folibRoleService.deleteRole(roleId);

        //同步角色信息到其他节点
        privilegeEventListenerRegistry.dispatchDeleteRoleSyncEvent(roleId);

        return getSuccessfulResponseEntity(SUCCESSFUL_DELETE_ROLE, accept);
    }

    @ApiOperation(value = "角色创建")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_CREATE_ROLE),
            @ApiResponse(code = 400, message = FAILED_CREATE_ROLE)})
    @PreAuthorize("hasAuthority('CREATE_ROLE')")
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity create(@RequestBody @Validated(RoleForm.NewRole.class) RoleForm roleForm,
                                      BindingResult bindingResult,
                                      Authentication authentication,
                                      @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_CREATE_ROLE, bindingResult);
        }
        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        String username = loggedUser.getUsername();

        RoleDTO roleDTO = RoleConvert.INSTANCE.formToDto(roleForm);
        if(roleDTO == null || roleDTO.getResources().isEmpty()) {
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, FAILED_CREATE_ROLE, accept);
        }
        folibRoleService.save(roleDTO, username);
        //同步角色信息到其他节点
        privilegeEventListenerRegistry.dispatchRoleSyncEvent(roleDTO.getName());
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
        RoleDTO roleDTO = folibRoleService.getRoleDetail(roleId, folibRole);

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
                                 @RequestBody @Validated(RoleForm.UpdateRole.class) RoleForm roleForm,
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
        folibRoleService.updateRoleInfo(roleDTO, roleId, username);

        //同步角色信息到其他节点
        privilegeEventListenerRegistry.dispatchRoleSyncEvent(roleId);
        return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE_ROLE, accept);
    }

    @ApiOperation(value = "Used to retrieve users")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GET_ROLE)})
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/queryRole", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public TableResultResponse<FolibRoleDTO> queryUser(@RequestParam(name = "page", required = false ,defaultValue ="1") Integer page,
                                                        @RequestParam(name = "limit", required = false,defaultValue = "10") Integer limit,
                                                       @RequestParam(name = "storageId", required = false) String storageId,
                                                       @RequestParam(name = "repositoryId", required = false) String repositoryId,
                                                       @RequestParam(name = "path", required = false) String path,
                                                        @RequestParam(name = "name", required = false) String name,
                                                       @RequestParam(name = "matchRoleName", required = false) String matchName,
                                                        @RequestParam(name = "isDefault", required = false) String isDefault) {
        PageRequest pageRequest = PageRequest.of(page, limit);
        FolibRole folibRole = FolibRole.builder().build();
        folibRole.setEnName(name);
        folibRole.setMatchEnName(matchName);
        folibRole.setIsDefault(isDefault);
        folibRole.setStorageId(storageId);
        folibRole.setRepositoryId(repositoryId);
        folibRole.setPath(path);

        PageInfo<FolibRoleDTO> folibRoles = folibRoleService.paginQuery(folibRole, pageRequest);
        if (Objects.isNull(folibRoles) || Objects.isNull(folibRoles.getList())) {
            return new TableResultResponse<>(0, null);
        }
        return new TableResultResponse<>(folibRoles.getTotal(), folibRoles.getList());

    }

    @ApiOperation(value = "Used to retrieve users")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GET_ROLE)})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping(value = "/privilegeDispatch", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity privilegeDispatch(@RequestBody @Validated PrivilegeDispatch dispatch, HttpServletRequest request, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        PrivilegeEventTypeEnum privilegeEventTypeEnum = dispatch.getPrivilegeEventTypeEnum();
        if (PrivilegeEventTypeEnum.EVENT_ALL_SYNC.getType() == privilegeEventTypeEnum.getType()) {
            syncAllPrivilege(dispatch.getTargetHostName());
        }else {
            //发送用户权限消息
            WSMessageRequest wsMessageRequest = null;
            WSMessageResponse messageResponse = null;
            try {
                //查询请求参数
                UserAuthDTO userAuthReq = getUserAuthReq(privilegeEventTypeEnum, dispatch.getUuId());
                wsMessageRequest = new WSMessageRequest(Command.USER_AUTH_SYNC, userAuthReq);
                messageResponse = folibWsRunManageV2.sendRequest(dispatch.getTargetHostName(), wsMessageRequest);

                log.debug("sendRequest result,wsMessageRequest:{},messageResponse:{}", wsMessageRequest, messageResponse);
            }  catch (Exception e) {
                log.error("sendRequest fail,wsMessageRequest:{}", wsMessageRequest, e);
            }

        }
        return ResponseEntity.ok("分发处理成功");
    }

    private void syncAllPrivilege(String targetHostName) {
        Map<String, ClusterDispatchNodeDto> map = configurationManagementService.
                getMutableConfigurationClone().getClusterDispatchNode();
        if (MapUtils.isEmpty(map)) {
            return;
        }
        final Collection<ClusterDispatchNodeDto> values = map.values();
        values.forEach(nodeDto -> {
            String targetHostName1 = FolibWsRunManageUtil.getSimpleTargetHostName(nodeDto);
            Session session = folibWsRunManageV2.getSession(targetHostName1);
            nodeDto.setWsClientOnline(session != null && session.isOpen());
        });
        map.forEach((key, dispatchNodeDto) -> {
            if (targetHostName.equalsIgnoreCase(dispatchNodeDto.getClusterNodeHost())) {
                Boolean isThisCluster = dispatchNodeDto.getIsThisCluster();
                Boolean wsClientOnline = dispatchNodeDto.getWsClientOnline();
                Boolean isSyncPrivilege = dispatchNodeDto.getIsSyncPrivilege();
                String syncStrategy = dispatchNodeDto.getSyncStrategy();
                Boolean autoRegister = dispatchNodeDto.getAutoRegister();

                if (!isThisCluster && !Objects.equals(wsClientOnline, null) && wsClientOnline
                            && !Objects.equals(isSyncPrivilege, null) && isSyncPrivilege) {
                    if (SyncStrategyEnum.TARGET_TO_SOURCE.getValue().equalsIgnoreCase(syncStrategy) && autoRegister){
                        syncAuthSourceToTarget(dispatchNodeDto);
                    }  else if (SyncStrategyEnum.SOURCE_TO_TARGET.getValue().equalsIgnoreCase(syncStrategy) && !autoRegister){
                        syncAuthSourceToTarget(dispatchNodeDto);
                    } else {
                        syncAuthSourceToTarget(dispatchNodeDto);
                    }
                    syncAuthSourceToTarget(dispatchNodeDto);
                }
            }
        });
        log.info("UserAuthSyncTask thread name [{}] time [{}]", Thread.currentThread().getName(), DateUtil.now());
    }

    private void syncAuthTargetToSource(String clusterNodeHost) {
        String baseUrl = configurationManagementService.getConfiguration().getBaseUrl();
        boolean dispatch = folibWsRunManageV2.dispatchTargetNode(clusterNodeHost, PrivilegeDispatch.builder().privilegeEventTypeEnum(PrivilegeEventTypeEnum.EVENT_ALL_SYNC).targetHostName(baseUrl).build());
        log.info("dispatch:{}", dispatch);
    }
    private void syncAuthSourceToTarget(ClusterDispatchNodeDto value) {
        WSMessageRequest wsMessageRequest = null;
        WSMessageResponse messageResponse = null;
        String clusterNodeHost = value.getClusterNodeHost();
        String targetHostName = FolibWsRunManageUtil.getTargetNode(clusterNodeHost);
        if (StringUtils.isBlank(targetHostName)) {
            //WS目标节点未找到，尝试转发到集群中其他节点处理
            targetHostName = FolibWsRunManageUtil.getTargetHostName(clusterNodeHost);
            log.info("targetHostName:{}", targetHostName);
            if (folibWsRunManageV2.dispatch(targetHostName, PrivilegeDispatch.builder().privilegeEventTypeEnum(PrivilegeEventTypeEnum.EVENT_ALL_SYNC).targetHostName(targetHostName).build())) {
                return;
            }
        }
        int page = 1;
        int size = 100;
        boolean flag = true;
        while (flag) {
            //发送用户权限消息
            try {
                //分页查询请求参数
                UserAuthDTO userAuthReq = getUserAuthReq(page, size);
                if (userAuthReq != null && userAuthReq.isNextPage()) {
                    page++;
                    size += 100;
                }else {
                    flag = false;
                }
                wsMessageRequest = new WSMessageRequest(Command.USER_AUTH_SYNC, userAuthReq);
                messageResponse = folibWsRunManageV2.sendRequest(targetHostName, wsMessageRequest);

                log.debug("sendRequest result,wsMessageRequest:{},messageResponse:{}", wsMessageRequest, messageResponse);

            }  catch (Exception e) {
                log.error("sendRequest fail,wsMessageRequest:{}", wsMessageRequest, e);
                flag = false;
            }
        }
    }

    private UserAuthDTO getUserAuthReq(int page, int size) {

        UserAuthDTO.UserAuthDTOBuilder builder = UserAuthDTO.builder();
        PageRequest pageRequest = PageRequest.of(page, size);
        //用户信息
        PageInfo<FolibUser> folibUserDTOS = folibUserService.paginQuery(FolibUser.builder().build(), pageRequest);
        List<FolibUser> userList = folibUserDTOS.getList();
        if (CollectionUtils.isNotEmpty(userList)) {
            builder.users(userList);
            builder.nextPage(true);
        }
        //用户组及用户组关联信息
        PageInfo<UserGroupListDTO> userGroupPageS = userGroupService.paginQuery(UserGroup.builder().build(), pageRequest);
        List<UserGroupListDTO> userGroupListDTOS = userGroupPageS.getList();
        if (!userGroupListDTOS.isEmpty()) {
            List<UserGroup> userGroups = UserGroupConvert.INSTANCE.UserGroupDTOToEntities(userGroupListDTOS);
            builder.groups(userGroups);
            List<Long> groupIds = userGroups.stream().map(UserGroup::getId).collect(Collectors.toList());
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
            if (!userGroupRefs.isEmpty()) {
                builder.userGroups(userGroupRefs);
            }
            builder.nextPage(true);
        }
        //角色信息及角色关联权限
        PageInfo<FolibRoleDTO> folibRoleDTOS = folibRoleService.paginQuery(FolibRole.builder().build(), pageRequest);
        List<FolibRoleDTO> roleDTOS = folibRoleDTOS.getList();
        if (!roleDTOS.isEmpty()) {
            List<FolibRole> folibRoles = RoleConvert.INSTANCE.roleDTOSToEntities(roleDTOS);
            builder.roles(folibRoles);
            List<String> roleIds = folibRoles.stream().map(FolibRole::getId).collect(Collectors.toList());
            if (!roleIds.isEmpty()) {
                List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryByRoleIds(roleIds);
                if (!roleResourceRefs.isEmpty()) {
                    builder.userRoles(roleResourceRefs);
                }
            }
            builder.nextPage(true);
        }
        //资源信息
        PageInfo<Resource> pageResource = resourceService.paginQuery(Resource.builder().build(), pageRequest);
        List<Resource> resources = pageResource.getList();
        if (CollectionUtils.isNotEmpty(resources)) {
            builder.resources(resources);
            builder.nextPage(true);
        }
        List<Resource> resourcesList = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getRepositoryId()) || StringUtils.isNotEmpty(resource.getStorageId())).collect(Collectors.toList());
        //仓库信息
        List<StorageDto> storages = new ArrayList<>();
        List<RepositoryDto> repositorys = new ArrayList<>();
        resourcesList.forEach(resource -> {
            String repositoryId = resource.getRepositoryId();
            String storageId = resource.getStorageId();
            if (StringUtils.isNotEmpty(repositoryId)) {
                StorageDto storage = configurationManagementService.getMutableConfigurationClone().getStorage(storageId);
                if (storage != null && storage.hasRepositories()) {
                    RepositoryDto repository = storage.getRepository(repositoryId);
                    if (repository != null && !repositorys.contains(repository)) {
                        repositorys.add(repository);
                    }
                }
            }else if (StringUtils.isNotEmpty(storageId)){
                StorageDto storage = configurationManagementService.getMutableConfigurationClone().getStorage(storageId);
                if (storage != null && !storages.contains(storage)) {
                    storages.add(storage);
                }
            }
        });
        if (!repositorys.isEmpty()){
            builder.repositorys(repositorys);
        }
        if (!storages.isEmpty()) {
            builder.storages(storages);
        }

        return builder.build();
    }

    private UserAuthDTO getUserAuthReq(PrivilegeEventTypeEnum privilegeEventTypeEnum, String uuId) {
        UserAuthDTO.UserAuthDTOBuilder builder = UserAuthDTO.builder();
        List<Resource> resourcesList = null;
        if (PrivilegeEventTypeEnum.EVENT_USER_SYNC.getType() == privilegeEventTypeEnum.getType() ) {
            UserDTO byUserName = folibUserService.findByUserName(uuId);
            if (Objects.nonNull(byUserName)) {
                builder.users(Collections.singletonList(UserConvert.INSTANCE.UserDTOToUser(byUserName)));
                Set<String> userGroupIds = byUserName.getUserGroupIds();
                List<Long> userGroupIdLs = userGroupIds.stream().map(Long::valueOf).collect(Collectors.toList());
                List<UserGroup> userGroups = userGroupService.queryByIds(userGroupIdLs);
                builder.groups(userGroups);
                if (CollectionUtils.isNotEmpty(userGroups)) {
                    List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(userGroupIdLs);
                    builder.userGroups(userGroupRefs);
                }
                Set<String> roles = byUserName.getRoles();
                builder.roles(folibRoleService.queryByIds(roles));
                if (CollectionUtils.isNotEmpty(roles)) {
                    List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryRefsByRoleIds(new ArrayList<>(roles));
                    builder.userRoles(roleResourceRefs);

                    if(CollectionUtils.isNotEmpty(roleResourceRefs)) {
                        List<String> resourceIds = roleResourceRefs.stream().map(RoleResourceRef::getResourceId).collect(Collectors.toList());
                        resourcesList = resourceService.queryByIds(resourceIds);
                        builder.resources(resourcesList);
                    }
                }


            }
        }


        if (PrivilegeEventTypeEnum.EVENT_USER_GROUP_SYNC.getType() == privilegeEventTypeEnum.getType() ) {
            UserGroup userGroup = userGroupService.queryById(Long.valueOf(uuId));
            builder.groups(Collections.singletonList(userGroup));
            if (!Objects.equals(userGroup, null)) {
                List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(Collections.singletonList(Long.valueOf(uuId)));
                builder.userGroups(userGroupRefs);

                List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryRefs(RoleResourceRef.builder().entityId(uuId).refType(GlobalConstants.ROLE_TYPE_USER_GROUP).build());
                if (CollectionUtils.isNotEmpty(roleResourceRefs)) {
                    builder.userRoles(roleResourceRefs);
                }
            }
        }
        if (PrivilegeEventTypeEnum.EVENT_ROLE_SYNC.getType() == privilegeEventTypeEnum.getType() ) {
            List<FolibRole> folibRoles = folibRoleService.queryByIds(Collections.singleton(uuId));
            builder.roles(folibRoles);

            if (CollectionUtils.isNotEmpty(folibRoles)) {
                List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryRefsByRoleIds(Collections.singletonList(uuId));
                builder.userRoles(roleResourceRefs);

                if(CollectionUtils.isNotEmpty(roleResourceRefs)) {
                    List<String> resourceIds = roleResourceRefs.stream().map(RoleResourceRef::getResourceId).collect(Collectors.toList());
                    resourcesList = resourceService.queryByIds(resourceIds);
                    builder.resources(resourcesList);

                    List<String> userIds = roleResourceRefs.stream().filter(roleResourceRef -> StringUtils.isNotBlank(roleResourceRef.getEntityId()) && GlobalConstants.ROLE_TYPE_USER.equals(roleResourceRef.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());
                    builder.users(folibUserService.queryByIds(userIds));


                    List<String> userGroupIds = roleResourceRefs.stream().filter(roleResourceRef -> StringUtils.isNotBlank(roleResourceRef.getEntityId()) && GlobalConstants.ROLE_TYPE_USER_GROUP.equals(roleResourceRef.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());
                    List<Long> userGroupIdLs = userGroupIds.stream().map(Long::valueOf).collect(Collectors.toList());
                    builder.groups(userGroupService.queryByIds(userGroupIdLs));

                    if (CollectionUtils.isNotEmpty(userGroupIdLs)) {
                        List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(userGroupIdLs);
                        builder.userGroups(userGroupRefs);
                    }
                }
            }
        }
        if (PrivilegeEventTypeEnum.EVENT_DELETE_USER_SYNC.getType() == privilegeEventTypeEnum.getType()) {
            builder.removeUserIds(Collections.singletonList(uuId));
        }
        if (PrivilegeEventTypeEnum.EVENT_DELETE_ROLE_SYNC.getType() == privilegeEventTypeEnum.getType()) {
            builder.removeRoleIds(Collections.singletonList(uuId));
        }
        if (PrivilegeEventTypeEnum.EVENT_DELETE_USER_GROUP_SYNC.getType() == privilegeEventTypeEnum.getType()) {
            builder.removeGroupIds(Collections.singletonList(Long.valueOf(uuId)));
        }
        if (PrivilegeEventTypeEnum.EVENT_DELETE_RESOURCE_SYNC.getType() == privilegeEventTypeEnum.getType()) {
            builder.removeResourceIds(Collections.singletonList(uuId));
        }


        if (CollectionUtils.isNotEmpty(resourcesList)) {
            List<StorageDto> storages = new ArrayList<>();
            List<RepositoryDto> repositorys = new ArrayList<>();

            resourcesList.forEach(resource -> {
                String repositoryId = resource.getRepositoryId();
                String storageId = resource.getStorageId();
                if (StringUtils.isNotEmpty(repositoryId)) {
                    repositorys.add(configurationManagementService.getMutableConfigurationClone().getStorage(storageId).getRepository(repositoryId));
                } else {
                    storages.add(configurationManagementService.getMutableConfigurationClone().getStorage(storageId));
                }
            });
            if (!repositorys.isEmpty()) {
                builder.repositorys(repositorys);
            }
            if (!storages.isEmpty()) {
                builder.storages(storages);
            }
        }

        return builder.build();
    }

}

