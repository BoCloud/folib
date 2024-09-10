package com.veadan.folib.task;


import cn.hutool.core.date.DateUtil;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.converters.users.RoleConvert;
import com.veadan.folib.converters.users.UserGroupConvert;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.PrivilegeDispatch;
import com.veadan.folib.dto.FolibRoleDTO;
import com.veadan.folib.dto.UserGroupListDTO;
import com.veadan.folib.entity.*;
import com.veadan.folib.event.privilege.PrivilegeEventTypeEnum;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.users.dto.UserAuthDTO;
import com.veadan.folib.users.service.*;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import com.veadan.folib.ws.server.Command;
import com.veadan.folib.ws.server.WSMessageRequest;
import com.veadan.folib.ws.server.WSMessageResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.websocket.Session;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * 同步用户权限定时task
 */
@Slf4j
@Component
@EnableScheduling
public class UserAuthSyncTask {

    @Autowired
    private DistributedLockComponent distributedLockComponent;
    @Inject
    @Lazy
    protected ConfigurationManagementService configurationManagementService;
    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;
    @Inject
    protected FolibUserService folibUserService;
    @Inject
    private UserGroupService userGroupService;
    @Inject
    private UserGroupRefService userGroupRefService;
    @Inject
    private RoleResourceRefService roleResourceRefService;
    @Inject
    private FolibRoleService folibRoleService;
    @Inject
    private ResourceService resourceService;

    @Scheduled(cron = "0 0/10 * * * ? ")
    public void run() {
        String lockName = "UserAuthSyncTask";
        long waitTime = 3L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
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
                map.forEach((key, value) -> {
                    log.debug("key:{},value:{}", key, value);
                    Boolean isThisCluster = value.getIsThisCluster();
                    Boolean wsClientOnline = value.getWsClientOnline();
                    Boolean isSyncPrivilege = value.getIsSyncPrivilege();

                    if (!isThisCluster && !value.getAutoRegister()
                            && !Objects.equals(wsClientOnline, null) && wsClientOnline
                            && !Objects.equals(isSyncPrivilege, null) && isSyncPrivilege) {
                        WSMessageRequest wsMessageRequest = null;
                        WSMessageResponse messageResponse = null;
                        String clusterNodeHost = value.getClusterNodeHost();
                        String targetHostName = FolibWsRunManageUtil.getTargetNode(clusterNodeHost);
                        if (StringUtils.isBlank(targetHostName)) {
                            //WS目标节点未找到，尝试转发到集群中其他节点处理
                            targetHostName = FolibWsRunManageUtil.getTargetHostName(clusterNodeHost);
                            if (folibWsRunManageV2.dispatch(targetHostName, PrivilegeDispatch.builder().privilegeEventTypeEnum(PrivilegeEventTypeEnum.EVENT_ALL_SYNC).targetHostName(targetHostName).build())) {
                                return ;
                            }
                        }
                        int page = 0;
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
                });
                log.info("UserAuthSyncTask thread name [{}] time [{}]", Thread.currentThread().getName(), DateUtil.now());
            } finally {
                distributedLockComponent.unLock(lockName, 3500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
    }

    private UserAuthDTO getUserAuthReq(int page, int size) {

        UserAuthDTO.UserAuthDTOBuilder builder = UserAuthDTO.builder();
        PageRequest pageRequest = PageRequest.of(page, size);
        //用户信息
        Page<FolibUser> folibUserDTOS = folibUserService.paginQuery(FolibUser.builder().build(), pageRequest);
        if (!folibUserDTOS.getContent().isEmpty()) {
            builder.users(new ArrayList<>(folibUserDTOS.getContent()));
            builder.nextPage(true);
        }
        //用户组及用户组关联信息
        Page<UserGroupListDTO> userGroupPageS = userGroupService.paginQuery(UserGroup.builder().build(), pageRequest);
        List<UserGroupListDTO> userGroupListDTOS = userGroupPageS.getContent();
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
        Page<FolibRoleDTO> folibRoleDTOS = folibRoleService.paginQuery(FolibRole.builder().build(), pageRequest);
        List<FolibRoleDTO> roleDTOS = folibRoleDTOS.getContent();
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
        Page<Resource> resources = resourceService.paginQuery(Resource.builder().build(), pageRequest);
        if (!resources.getContent().isEmpty()) {
            builder.resources(new ArrayList<>(resources.getContent()));
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
}
