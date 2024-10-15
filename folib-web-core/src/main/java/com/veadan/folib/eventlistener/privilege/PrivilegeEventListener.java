package com.veadan.folib.eventlistener.privilege;

import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.converts.UserConvert;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.PrivilegeDispatch;
import com.veadan.folib.dto.UserDTO;
import com.veadan.folib.entity.*;
import com.veadan.folib.enums.SyncStrategyEnum;
import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.event.privilege.PrivilegeEvent;
import com.veadan.folib.event.privilege.PrivilegeEventTypeEnum;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.users.dto.UserAuthDTO;
import com.veadan.folib.users.service.*;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import com.veadan.folib.ws.server.Command;
import com.veadan.folib.ws.server.WSMessageRequest;
import com.veadan.folib.ws.server.WSMessageResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.websocket.Session;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * 事件监听，处理制品缓存
 **/
@Slf4j
@Component
public class PrivilegeEventListener {

    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;
    @Autowired
    @Lazy
    protected ConfigurationManagementService configurationManagementService;
    @Autowired
    private FolibUserService folibUserService;
    @Autowired
    private FolibRoleService folibRoleService;
    @Autowired
    private UserGroupService userGroupService;
    @Autowired
    private UserGroupRefService userGroupRefService;
    @Autowired
    private RoleResourceRefService roleResourceRefService;
    @Autowired
    private ResourceService resourceService;

    @AsyncEventListener
    public void handle(final PrivilegeEvent event) {
        long startTime = System.currentTimeMillis();
        int source = (int) event.getSource();
        String uuId = event.getUuId();
        PrivilegeEventTypeEnum privilegeEventTypeEnum = PrivilegeEventTypeEnum.queryPrivilegeEventTypeEnumByType(source);
        log.debug("监听到权限同步事件 [{}]，主键Id [{}]", privilegeEventTypeEnum, uuId);
        if (Objects.isNull(privilegeEventTypeEnum)) {
            return;
        }

        try {

            Map<String, ClusterDispatchNodeDto> map = configurationManagementService.
                    getMutableConfigurationClone().getClusterDispatchNode();
            if (MapUtils.isEmpty(map)) {
                return;
            }
            final Collection<ClusterDispatchNodeDto> values = map.values();
            values.forEach(nodeDto -> {
                String targetHostName = FolibWsRunManageUtil.getSimpleTargetHostName(nodeDto);
                Session session = folibWsRunManageV2.getSession(targetHostName);
                nodeDto.setWsClientOnline(session != null && session.isOpen());
            });

            map.forEach((key, dispatchNodeDto) -> {
                log.debug("key:{},dispatchNodeDto:{}", key, dispatchNodeDto);
                Boolean isThisCluster = dispatchNodeDto.getIsThisCluster();
                Boolean wsClientOnline = dispatchNodeDto.getWsClientOnline();
                Boolean isSyncPrivilege = dispatchNodeDto.getIsSyncPrivilege();
                String syncStrategy = dispatchNodeDto.getSyncStrategy();
                String clusterNodeHost = dispatchNodeDto.getClusterNodeHost();
                Boolean autoRegister = dispatchNodeDto.getAutoRegister();

                log.info("isThisCluster:{},wsClientOnline:{},isSyncPrivilege:{},syncStrategy:{},clusterNodeHost:{}", isThisCluster, wsClientOnline, isSyncPrivilege, syncStrategy, clusterNodeHost);

                if (!isThisCluster
                        && !Objects.equals(wsClientOnline, null) && wsClientOnline
                        && !Objects.equals(isSyncPrivilege, null) && isSyncPrivilege){
                    if (SyncStrategyEnum.TARGET_TO_SOURCE.getValue().equalsIgnoreCase(syncStrategy) && autoRegister){
                        syncAuthSourceToTarget(dispatchNodeDto, privilegeEventTypeEnum, uuId);
                    }  else if (SyncStrategyEnum.SOURCE_TO_TARGET.getValue().equalsIgnoreCase(syncStrategy) && !autoRegister){
                        syncAuthSourceToTarget(dispatchNodeDto, privilegeEventTypeEnum, uuId);
                    } else {
                        syncAuthSourceToTarget(dispatchNodeDto, privilegeEventTypeEnum, uuId);
                    }
                }
            });

        } catch (Exception ex) {
            log.error("事件监听，处理backup，事件类型：{} 主键id：{} 错误：{}", source, uuId, ExceptionUtils.getStackTrace(ex));
        }
    }

    private void syncAuthTargetToSource(String clusterNodeHost, PrivilegeEventTypeEnum privilegeEventTypeEnum, String uuId) {
        String baseUrl = configurationManagementService.getConfiguration().getBaseUrl();
        boolean dispatch = folibWsRunManageV2.dispatchTargetNode(clusterNodeHost, PrivilegeDispatch.builder().privilegeEventTypeEnum(privilegeEventTypeEnum).targetHostName(baseUrl).uuId(uuId).build());
        log.info("dispatch:{}", dispatch);
    }
    private void syncAuthSourceToTarget(ClusterDispatchNodeDto value, PrivilegeEventTypeEnum privilegeEventTypeEnum, String uuId) {
        WSMessageRequest wsMessageRequest = null;
        WSMessageResponse messageResponse = null;
        String clusterNodeHost = value.getClusterNodeHost();
        String targetHostName = FolibWsRunManageUtil.getTargetNode(clusterNodeHost);
        if (StringUtils.isBlank(targetHostName)) {
            //WS目标节点未找到，尝试转发到集群中其他节点处理
            targetHostName = FolibWsRunManageUtil.getTargetHostName(clusterNodeHost);
            if (folibWsRunManageV2.dispatch(targetHostName, PrivilegeDispatch.builder().targetHostName(targetHostName).privilegeEventTypeEnum(privilegeEventTypeEnum).uuId(uuId).build())) {
                return;
            }
        }

        //发送用户权限消息
        try {
            //查询请求参数
            UserAuthDTO userAuthReq = getUserAuthReq(privilegeEventTypeEnum, uuId);
            wsMessageRequest = new WSMessageRequest(Command.USER_AUTH_SYNC, userAuthReq);
            messageResponse = folibWsRunManageV2.sendRequest(targetHostName, wsMessageRequest);
//                        SpringContextUtil.getBeanWithAnnotation(RelationalDatabaseUserService.RelationalDatabase.class, RelationalDatabaseUserService.class).syncUserAuth(userAuthReq);
            log.debug("sendRequest result,wsMessageRequest:{},messageResponse:{}", wsMessageRequest, messageResponse);
        }  catch (Exception e) {
            log.error("sendRequest fail,wsMessageRequest:{}", wsMessageRequest, e);
        }
    }

    private UserAuthDTO getUserAuthReq(PrivilegeEventTypeEnum privilegeEventTypeEnum, String uuId) {
        UserAuthDTO.UserAuthDTOBuilder builder = UserAuthDTO.builder();
        List<Resource> resourcesList = new ArrayList<>();
        if (PrivilegeEventTypeEnum.EVENT_USER_SYNC.getType() == privilegeEventTypeEnum.getType() ) {
            UserDTO byUserName = folibUserService.findByUserName(uuId);
            if (Objects.nonNull(byUserName)) {
                builder.users(Collections.singletonList(UserConvert.INSTANCE.UserDTOToUser(byUserName)));
                Set<String> userGroupIds = byUserName.getUserGroupIds();
                if (CollectionUtils.isNotEmpty(userGroupIds)) {
                    List<Long> userGroupIdLs = userGroupIds.stream().map(Long::valueOf).collect(Collectors.toList());
                    List<UserGroup> userGroups = userGroupService.queryByIds(userGroupIdLs);
                    builder.groups(userGroups);
                    if (CollectionUtils.isNotEmpty(userGroups)) {
                        List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(userGroupIdLs);
                        builder.userGroups(userGroupRefs);
                    }
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
                    if (CollectionUtils.isNotEmpty(resourceIds)) {
                        resourcesList = resourceService.queryByIds(resourceIds);
                        builder.resources(resourcesList);
                    }


                    List<String> userIds = roleResourceRefs.stream().filter(roleResourceRef -> StringUtils.isNotBlank(roleResourceRef.getEntityId()) && GlobalConstants.ROLE_TYPE_USER.equals(roleResourceRef.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(userIds)) {
                        builder.users(folibUserService.queryByIds(userIds));
                    }


                    List<String> userGroupIds = roleResourceRefs.stream().filter(roleResourceRef -> StringUtils.isNotBlank(roleResourceRef.getEntityId()) && GlobalConstants.ROLE_TYPE_USER_GROUP.equals(roleResourceRef.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());
                    List<Long> userGroupIdLs = userGroupIds.stream().map(Long::valueOf).collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(userGroupIdLs)) {
                        builder.groups(userGroupService.queryByIds(userGroupIdLs));
                    }

                    if (CollectionUtils.isNotEmpty(userGroupIdLs)) {
                        List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(userGroupIdLs);
                        builder.userGroups(userGroupRefs);
                    }
                }
            }
        }

        if (PrivilegeEventTypeEnum.EVENT_RESOURCE_SYNC.getType() == privilegeEventTypeEnum.getType()) {
            Resource resource = resourceService.queryById(uuId.toUpperCase());
            if (resource != null) {
                resourcesList.add(resource);
                if (StringUtils.isNotEmpty(resource.getRepositoryId())) {
                    resourcesList.add(Resource.builder().storageId(resource.getStorageId()).id(resource.getStorageId().toUpperCase()).build());
                }
                builder.resources(resourcesList);

                List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryByResourceIds(Collections.singletonList(uuId));
                if (CollectionUtils.isNotEmpty(roleResourceRefs)) {
                    builder.userRoles(roleResourceRefs);

                    Set<String> roleIds = roleResourceRefs.stream().map(RoleResourceRef::getRoleId).distinct().collect(Collectors.toSet());
                    roleIds.addAll(Set.of(String.format("STORAGE_ADMIN_%S", uuId), String.format("STORAGE_USER_%S", uuId)));
                    List<FolibRole> folibRoles = folibRoleService.queryByIds(roleIds);
                    if (CollectionUtils.isNotEmpty(folibRoles)) {
                        builder.roles(folibRoles);
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
