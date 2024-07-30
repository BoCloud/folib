package com.veadan.folib.controllers.configuration;

import cn.hutool.core.collection.CollectionUtil;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.annotation.JsonView;
import com.google.common.collect.Lists;
import com.veadan.folib.annotation.LicenseAnnotation;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.cluster.SyncAuthorizationEnum;
import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.cluster.SyncStorageEnum;
import com.veadan.folib.cluster.SyncUnionRepositoryEnum;
import com.veadan.folib.components.common.CommonComponent;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.config.PermissionCheck;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.controllers.cluster.dto.SyncAuthorizationDto;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.controllers.cluster.dto.SyncStorageDto;
import com.veadan.folib.controllers.cluster.dto.SyncUnionRepositoryDto;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.*;
import com.veadan.folib.dto.ArtifactDispatchRepositoryDto;
import com.veadan.folib.enums.ArtifactoryRepositoryTypeEnum;
import com.veadan.folib.enums.NotifyScopesTypeEnum;
import com.veadan.folib.enums.RepositoryScopeEnum;
import com.veadan.folib.enums.StorageProviderEnum;
import com.veadan.folib.event.repository.RepositoryEventListenerRegistry;
import com.veadan.folib.forms.common.StorageTreeForm;
import com.veadan.folib.forms.configuration.*;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.providers.storage.FileSystemStorageProvider;
import com.veadan.folib.repository.RepositoryManagementStrategyException;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.services.support.ConfigurationException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageData;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.Views;
import com.veadan.folib.storage.repository.*;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.domain.Users;
import com.veadan.folib.users.dto.PathPrivilegesDto;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.DatabaseUserService;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.validation.RequestBodyValidationException;
import com.veadan.folib.web.RepositoryMapping;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import com.veadan.folib.ws.server.Command;
import com.veadan.folib.ws.server.WSMessageRequest;
import com.veadan.folib.ws.server.WSMessageResponse;
import io.swagger.annotations.*;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.glassfish.jersey.client.ClientProperties;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.core.convert.ConversionService;
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
import javax.validation.groups.Default;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.nio.file.Files;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
@RestController
@RequestMapping("/api/configuration/folib/storages")
@Api(description = "存储控件设置", tags = "存储控件设置")
public class StoragesConfigurationController
        extends BaseConfigurationController {
    static final String SUCCESSFUL_SAVE_STORAGE = "The storage was created successfully.";

    static final String FAILED_SAVE_STORAGE_FORM_ERROR = "The storage cannot be created because the submitted form contains errors!";

    static final String FAILED_SAVE_STORAGE_ERROR = "The storage was not created.";

    static final String SUCCESSFUL_UPDATE_STORAGE = "The storage was updated successfully.";

    static final String FAILED_UPDATE_STORAGE_FORM_ERROR = "The storage cannot be updated because the submitted form contains errors!";

    static final String FAILED_UPDATE_STORAGE_ERROR = "The storage was not updated.";

    static final String FAILED_SAVE_REPOSITORY = "The repository cannot be saved because the submitted form contains errors!";

    static final String FAILED_SAVE_REPOSITORY_PERMISSION = "The repository permission cannot be saved because the submitted form contains errors!";

    static final String FAILED_SAVE_REPOSITORY_PERMISSION_USER = "仓库可见范围改为存储空间内，用户【%s】不属于该存储空间，需要先从授权列表中移除";

    static final String SUCCESSFUL_REPOSITORY_SAVE = "The repository was updated successfully.";

    static final String FAILED_REPOSITORY_SAVE = "The repository was not saved.";

    static final String SUCCESSFUL_STORAGE_REMOVAL = "The storage was removed successfully.";

    static final String SUCCESSFUL_REPOSITORY_REMOVAL = "The repository was removed successfully.";

    private static final String FAILED_STORAGE_REMOVAL = "Failed to remove the storage !";

    private static final String STORAGE_NOT_FOUND = "The storage was not found.";

    private static final String FAILED_REPOSITORY_REMOVAL = "Failed to remove the repository !";

    private static final String FAILED_STORAGE_REMOVAL_EXISTS_REPOSITORY = "删除存储空间失败，请先删除其下的仓库 !";

    private static final String FAILED_REPOSITORY_REMOVAL_EXISTS_GROUP_REPOSITORY = "删除仓库失败，存在关联组合库，请先从组合库[%s]中移除 !";

    private static final String PARAMS_ERROR = "参数错误，请检查参数！";

    private final StorageManagementService storageManagementService;

    private final RepositoryManagementService repositoryManagementService;

    private final ConversionService conversionService;

    @Autowired
    private ClusterSyncService clusterSyncService;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private RepositoryEventListenerRegistry repositoryEventListenerRegistry;

    @Autowired
    @DatabaseUserService.Database
    @Lazy
    private UserService userService;

    @Autowired
    private AuthorizationConfigService authorizationConfigService;

    @Autowired
    private SecurityComponent securityComponent;

    @Autowired
    private CommonComponent commonComponent;
    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;

    @Inject
    @Lazy
    private DockerLayoutProvider dockerLayoutProvider;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;


    public StoragesConfigurationController(ConfigurationManagementService configurationManagementService,
                                           StorageManagementService storageManagementService,
                                           RepositoryManagementService repositoryManagementService,
                                           ConversionService conversionService) {
        super(configurationManagementService);
        this.storageManagementService = storageManagementService;
        this.repositoryManagementService = repositoryManagementService;
        this.conversionService = conversionService;
    }

    @LicenseAnnotation
    @ApiOperation(value = "Adds a storage.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The storage was created successfully."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_STORAGE')")
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity createStorage(@RequestBody
                                        @Validated({Default.class,
                                                StorageForm.NewStorage.class,
                                                ProxyConfigurationForm.ProxyConfigurationFormChecks.class})
                                                StorageForm storageForm,
                                        BindingResult bindingResult,
                                        @RequestHeader(HttpHeaders.ACCEPT)
                                                String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_SAVE_STORAGE_FORM_ERROR, bindingResult);
        }
        try {
            StorageDto storage = conversionService.convert(storageForm, StorageDto.class);
            if (StringUtils.isBlank(storage.getAdmin())) {
                storage.setAdmin(NotifyScopesTypeEnum.ADMIN.getScope());
            }
            if (StringUtils.isBlank(storage.getStorageProvider())) {
                storage.setStorageProvider(StorageProviderEnum.LOCAL.getType());
            }
            storageManagementService.createStorage(storage);
            // 向其他集群节点同步storage
            SyncStorageDto syncStorageDto = new SyncStorageDto(storage, storageForm.getId(), SyncStorageEnum.CREATE);
            clusterSyncService.syncStorage(syncStorageDto);
            AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
            SyncAuthorizationDto syncAuthorizationDto = new SyncAuthorizationDto(authorizationConfigDto, SyncAuthorizationEnum.UPDATE);
            clusterSyncService.syncAuthorization(syncAuthorizationDto);
            return getSuccessfulResponseEntity(SUCCESSFUL_SAVE_STORAGE, accept);
        } catch (ConfigurationException | IOException e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_SAVE_STORAGE_ERROR, e, accept);
        }
    }

    @ApiOperation(value = "Updates a storage.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The storage was updated successfully."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_STORAGE')")
    @PutMapping(value = "{storageId}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity updateStorage(
            @ApiParam(value = "The storageId", required = true)
            @PathVariable String storageId,
            @RequestBody @Validated({Default.class,
                    StorageForm.ExistingStorage.class,
                    ProxyConfigurationForm.ProxyConfigurationFormChecks.class}) StorageForm storageFormToUpdate,
            BindingResult bindingResult,
            @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_UPDATE_STORAGE_FORM_ERROR, bindingResult);
        }

        if (!StringUtils.equals(storageId, storageFormToUpdate.getId())) {
            return getNotFoundResponseEntity(FAILED_UPDATE_STORAGE_ERROR, accept);
        }

        try {
            StorageDto storage = conversionService.convert(storageFormToUpdate, StorageDto.class);
            if (StringUtils.isBlank(storage.getAdmin())) {
                storage.setAdmin(NotifyScopesTypeEnum.ADMIN.getScope());
            }
            storageManagementService.updateStorage(storage);
            SyncStorageDto syncStorageDto = new SyncStorageDto(storage, storageId, SyncStorageEnum.UPDATE);
            clusterSyncService.syncStorage(syncStorageDto);
            AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
            SyncAuthorizationDto syncAuthorizationDto = new SyncAuthorizationDto(authorizationConfigDto, SyncAuthorizationEnum.UPDATE);
            clusterSyncService.syncAuthorization(syncAuthorizationDto);
            return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE_STORAGE, accept);
        } catch (ConfigurationException | IOException e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_UPDATE_STORAGE_ERROR, e, accept);
        }
    }

    @JsonView(Views.ShortStorage.class)
    @ApiOperation(value = "Retrieve the basic info about storages.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getStorages(Authentication authentication) {
        final List<Storage> storages = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());
        String username = "";
        if (Objects.nonNull(authentication)) {
            final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
            username = loggedUser.getUsername();
        }
        StoragesOutput storagesOutput = new StoragesOutput(storages);
        if (!hasAdmin()) {
            List<Storage> list = storagesOutput.getStorages();
            String finalUsername = username;
            List<Storage> collect = list.stream().filter(s ->
                    (CollectionUtil.isNotEmpty(s.getUsers()) && s.getUsers().contains(finalUsername)) ||
                            (CollectionUtils.isNotEmpty(s.getRepositories().values()) && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope())))
            ).collect(Collectors.toList());
            storagesOutput.setStorages(collect);
        }
        return ResponseEntity.ok(storagesOutput);
    }

    @ApiOperation(value = "Retrieve the basic info about storages and repositories.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/getStoragesAndRepositories", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getStoragesAndRepositories(@ApiParam(value = "Search for repository names in a specific storageId")
                                                     @RequestParam(value = "storageId", required = false)
                                                             String storageId,
                                                     @ApiParam(value = "Filter repository names by type (i.e. hosted, group, proxy)")
                                                     @RequestParam(value = "type", required = false)
                                                             String type,
                                                     @ApiParam(value = "Filter exclude repository names by type (i.e. hosted, group, proxy)")
                                                     @RequestParam(value = "excludeType", required = false)
                                                             String excludeType,
                                                     @ApiParam(value = "Search for exclude repository names")
                                                     @RequestParam(value = "excludeRepositoryId", required = false)
                                                             String excludeRepositoryId,
                                                     @ApiParam(value = "Filter repository names by repository layout")
                                                     @RequestParam(value = "layout", required = false)
                                                             String layout,
                                                     @ApiParam(value = "Filter repository names by repository policy")
                                                     @RequestParam(value = "policy", required = false)
                                                             String policy, Authentication authentication) {
        List<Storage> storages = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());
        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        String username = loggedUser.getUsername();
        List<StorageTreeForm> storageTreeForms = Lists.newArrayList();
        if (CollectionUtil.isNotEmpty(storages)) {
            boolean filterByUser = !hasAdmin();
            boolean filterByStorageId = StringUtils.isNotBlank(storageId);
            boolean filterByType = StringUtils.isNotBlank(type);
            boolean filterByLayout = StringUtils.isNotBlank(layout);
            boolean filterByExcludeRepositoryId = StringUtils.isNotBlank(excludeRepositoryId);
            boolean filterByExcludeType = StringUtils.isNotBlank(excludeType);
            boolean filterByPolicy = StringUtils.isNotBlank(policy);
            storages = storages.stream()
                    .distinct()
                    .filter(s -> !filterByUser || (CollectionUtil.isNotEmpty(s.getUsers()) && s.getUsers().contains(loggedUser.getUsername())) ||
                            (CollectionUtils.isNotEmpty(s.getRepositories().values()) && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()))))
                    .filter(s -> !filterByStorageId || s.getId().equalsIgnoreCase(storageId))
                    .collect(Collectors.toCollection(LinkedList::new));
            StorageTreeForm storageTreeForm;
            List<Repository> repositories;
            for (Storage storage : storages) {
                boolean flag = !hasAdmin() && !username.equals(storage.getAdmin()) && (CollectionUtils.isNotEmpty(storage.getUsers()) && !storage.getUsers().contains(username));
                storageTreeForm = StorageTreeForm.builder().id(storage.getId()).key(storage.getId()).name(storage.getId()).build();
                repositories = new LinkedList<Repository>(storage.getRepositories().values());
                repositories = repositories.stream().distinct()
                        .filter(r -> !filterByType || r.getType().equalsIgnoreCase(type))
                        .filter(r -> !filterByLayout || r.getLayout().equalsIgnoreCase(layout))
                        .filter(r -> !filterByPolicy || r.getPolicy().equalsIgnoreCase(policy))
                        .filter(r -> !filterByExcludeRepositoryId || !r.getId().equalsIgnoreCase(excludeRepositoryId))
                        .filter(r -> !filterByExcludeType || !r.getType().equalsIgnoreCase(excludeType))
                        .collect(Collectors.toCollection(LinkedList::new));
                if (flag) {
                    repositories = repositories.stream().filter((item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()))).collect(Collectors.toList());
                }
                storageTreeForm.setChildren(repositories.stream().map(repository -> StorageTreeForm.builder().id(repository.getId()).key(storage.getId() + "," + repository.getId()).name(repository.getId()).type(repository.getType()).layout(repository.getLayout())
                        .scope(repository.getScope()).build()).collect(Collectors.toList()));
                storageTreeForms.add(storageTreeForm);
            }
        }
        return ResponseEntity.ok(storageTreeForms);
    }

    @ApiOperation(value = "Retrieve the basic info about storages and repositories.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/getPermissionStoragesAndRepositories", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getPermissionStoragesAndRepositories(
            @ApiParam(value = "Filter repository names by type (i.e. hosted, group, proxy)")
            @RequestParam(value = "type", required = false)
                    String type,
            @ApiParam(value = "Search for exclude repository names")
            @RequestParam(value = "excludeRepositoryId", required = false)
                    String excludeRepositoryId,
            @ApiParam(value = "Filter repository names by repository layout")
            @RequestParam(value = "layout", required = false)
                    String layout,
            @ApiParam(value = "Filter repository names by repository policy")
            @RequestParam(value = "policy", required = false)
                    String policy, Authentication authentication) {
        List<Storage> storages = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());
        final SpringSecurityUser loggedUser = (SpringSecurityUser) authentication.getPrincipal();
        String username = loggedUser.getUsername();
        List<StorageTreeForm> storageTreeForms = Lists.newArrayList();
        if (CollectionUtil.isNotEmpty(storages)) {
            boolean filterByUser = !hasAdmin() && loggedUser.getRoles().stream().noneMatch(role -> SystemRole.ARTIFACTS_MANAGER.name().equals(role.getName()));
            boolean filterByType = StringUtils.isNotBlank(type);
            boolean filterByLayout = StringUtils.isNotBlank(layout);
            boolean filterByExcludeRepositoryId = StringUtils.isNotBlank(excludeRepositoryId);
            boolean filterByPolicy = StringUtils.isNotBlank(policy);
            storages = storages.stream()
                    .distinct()
                    .filter(s -> !filterByUser || username.equals(s.getAdmin()))
                    .collect(Collectors.toCollection(LinkedList::new));
            StorageTreeForm storageTreeForm;
            List<Repository> repositories;
            for (Storage storage : storages) {
                boolean flag = !hasAdmin() && !username.equals(storage.getAdmin()) && (CollectionUtils.isNotEmpty(storage.getUsers()) && !storage.getUsers().contains(username));
                storageTreeForm = StorageTreeForm.builder().id(storage.getId()).key(storage.getId()).name(storage.getId()).build();
                repositories = new LinkedList<Repository>(storage.getRepositories().values());
                repositories = repositories.stream().distinct()
                        .filter(r -> !filterByType || r.getType().equalsIgnoreCase(type))
                        .filter(r -> !filterByLayout || r.getLayout().equalsIgnoreCase(layout))
                        .filter(r -> !filterByPolicy || r.getPolicy().equalsIgnoreCase(policy))
                        .filter(r -> !filterByExcludeRepositoryId || !r.getId().equalsIgnoreCase(excludeRepositoryId))
                        .collect(Collectors.toCollection(LinkedList::new));
                if (flag) {
                    repositories = repositories.stream().filter((item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()))).collect(Collectors.toList());
                }
                storageTreeForm.setChildren(repositories.stream().map(repository -> StorageTreeForm.builder().id(repository.getId()).key(storage.getId() + "," + repository.getId()).name(repository.getId()).type(repository.getType()).layout(repository.getLayout()).build()).collect(Collectors.toList()));
                storageTreeForms.add(storageTreeForm);
            }
        }
        return ResponseEntity.ok(storageTreeForms);
    }

    @PostMapping(value = "/getDispatchRepositories", produces = MediaType.APPLICATION_JSON_VALUE)
    @PermissionCheck(resourceKey = "ARTIFACTS_VIEW")
    public ResponseEntity<DispatchStorageTree> getDispatchRepositories(@RequestBody ArtifactDispatchRepositoryDto dispatchRepositoryDto) {
        String dispatchEnName = dispatchRepositoryDto.getDispatchEnName();
        String type = dispatchRepositoryDto.getType();
        String layout = dispatchRepositoryDto.getLayout();
        String policy = dispatchRepositoryDto.getPolicy();
        List<Storage> storages = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());
        List<StorageTreeForm> dispatchTreeForms = Lists.newArrayList();
        List<StorageTreeForm> storageTreeForms = Lists.newArrayList();
        StorageTreeForm dispatchTreeForm = StorageTreeForm.builder()
                .id(dispatchEnName)
                .key(dispatchEnName)
                .name(dispatchEnName).artifactoryRepositoryType(ArtifactoryRepositoryTypeEnum.INNER.getType()).build();
        if (CollectionUtil.isNotEmpty(storages)) {
            boolean filterByType = StringUtils.isNotBlank(type);
            boolean filterByLayout = StringUtils.isNotBlank(layout);
            boolean filterByPolicy = StringUtils.isNotBlank(policy);
            storages = storages.stream()
                    .distinct()
                    .collect(Collectors.toCollection(LinkedList::new));
            List<Repository> repositories;
            for (Storage storage : storages) {
                StorageTreeForm storageTreeForm = StorageTreeForm.builder()
                        .id(storage.getId())
                        .key(dispatchEnName + "," + storage.getId())
                        .name(storage.getId()).artifactoryRepositoryType(ArtifactoryRepositoryTypeEnum.INNER.getType()).build();
                repositories = new LinkedList<Repository>(storage.getRepositories().values());
                repositories = repositories.stream().distinct()
                        .filter(r -> !filterByType || r.getType().equalsIgnoreCase(type))
                        .filter(r -> !filterByLayout || r.getLayout().equalsIgnoreCase(layout))
                        .filter(r -> !filterByPolicy || r.getPolicy().equalsIgnoreCase(policy))
                        .collect(Collectors.toCollection(LinkedList::new));
                if (repositories.size() == 0) {
                    continue;
                }
                storageTreeForm.setChildren(repositories.stream().map(repository ->
                        StorageTreeForm.builder()
                                .id(repository.getId())
                                .key(dispatchEnName + "," + storage.getId() + "," + repository.getId())
                                .name(repository.getId())
                                .type(repository.getType())
                                .layout(repository.getLayout())
                                .artifactoryRepositoryType(ArtifactoryRepositoryTypeEnum.INNER.getType())
                                .build()).collect(Collectors.toList()));
                storageTreeForms.add(storageTreeForm);
            }
            dispatchTreeForm.setChildren(storageTreeForms);
            dispatchTreeForms.add(dispatchTreeForm);
        }
        return ResponseEntity.ok(new DispatchStorageTree(dispatchTreeForms));
    }


    @ApiOperation(value = "Retrieve the basic info about storages and repositories.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PermissionCheck(resourceKey = "ARTIFACTS_VIEW")
    @GetMapping(value = "/getDispatchStoragesAndRepositories", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getDispatchStoragesAndRepositories(@ApiParam(value = "Search for repository names in a specific storageId")
                                                             @RequestParam(value = "storageId", required = false)
                                                                     String storageId,
                                                             @ApiParam(value = "Filter repository names by type (i.e. hosted, group, proxy)")
                                                             @RequestParam(value = "type", required = false)
                                                                     String type,
                                                             @ApiParam(value = "Search for exclude repository names")
                                                             @RequestParam(value = "excludeRepositoryId", required = false)
                                                                     String excludeRepositoryId,
                                                             @ApiParam(value = "Filter repository names by repository layout")
                                                             @RequestParam(value = "layout", required = false)
                                                                     String layout,
                                                             @ApiParam(value = "Filter repository names by repository policy")
                                                             @RequestParam(value = "policy", required = false)
                                                                     String policy, Authentication authentication) {

        logger.info("start getDispatchStoragesAndRepositories");
        // 获取制品分发配置列表(非本集群)
        Map<String, ClusterDispatchNodeDto> map = configurationManagementService.
                getMutableConfigurationClone().getClusterDispatchNode();
        List<ClusterDispatchNodeDto> listDispatch =
                map.values().stream().filter(x -> !x.getIsThisCluster()).collect(Collectors.toList());
        ArtifactDispatchRepositoryDto dispatchRepositoryDto = ArtifactDispatchRepositoryDto.builder()
                .type(type)
                .layout(layout)
                .policy(policy).build();
        List<StorageTreeForm> repoList = new LinkedList<>();
        WSMessageRequest wsMessageRequest = null;
        WSMessageResponse messageResponse = null;
        for (ClusterDispatchNodeDto clusterDispatchNodeDto : listDispatch) {
            try {
                final String dispatchEnName = clusterDispatchNodeDto.getClusterEnName();
                dispatchRepositoryDto.setDispatchEnName(dispatchEnName);
                wsMessageRequest = new WSMessageRequest(Command.STORAGES_REPOSITORY_TREE, dispatchRepositoryDto);
                String targetHostName = FolibWsRunManageUtil.getTargetNode(clusterDispatchNodeDto.getClusterNodeHost());
                if (StringUtils.isBlank(targetHostName)) {
                    //WS目标节点未找到，尝试转发到集群中其他节点处理
                    targetHostName = FolibWsRunManageUtil.getTargetHostName(clusterDispatchNodeDto.getClusterNodeHost());
                    if (folibWsRunManageV2.forward(targetHostName)) {
                        return null;
                    }
                }
                messageResponse = folibWsRunManageV2.sendRequest(targetHostName, wsMessageRequest);
                DispatchStorageTree dispatchStorageTree = (DispatchStorageTree) messageResponse.getDate();
                repoList.addAll(dispatchStorageTree.getList());
            } catch (Exception e) {
                logger.warn("sendRequest fail,wsMessageRequest:{}", wsMessageRequest, e);
            }
        }
        // 发送获取仓库信息Task
        return ResponseEntity.ok(repoList);
    }


    @JsonView(Views.LongStorage.class)
    @ApiOperation(value = "Retrieve the configuration of a storage.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "The storage ${storageId} was not found.")})
    @PreAuthorize("hasAnyAuthority('CONFIGURATION_VIEW_STORAGE_CONFIGURATION', 'ARTIFACTS_VIEW')")
    @GetMapping(value = "/{storageId}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getStorageResponseEntity(@ApiParam(value = "The storageId", required = true)
                                                   @PathVariable final String storageId,
                                                   @ApiParam(value = "The filter")
                                                   @RequestParam(value = "filter", required = false) Boolean filter) {
        StorageDto storage = configurationManagementService.getMutableConfigurationClone().getStorage(storageId);
        if (storage != null) {
            String username = loginUsername();
            boolean flag = Boolean.TRUE.equals(filter) && !hasAdmin() && !username.equals(storage.getAdmin()) && (CollectionUtils.isEmpty(storage.getUsers()) || (CollectionUtils.isNotEmpty(storage.getUsers()) && !storage.getUsers().contains(username)));
            if (flag) {
                Map<String, ? extends Repository> repositoryMap = storage.getRepositories();
                if (Objects.nonNull(repositoryMap) && CollectionUtils.isNotEmpty(repositoryMap.values())) {
                    repositoryMap = repositoryMap.values().stream().filter(item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope())).collect(Collectors.toMap(Repository::getId, Function.identity()));
                    storage.setRepositories((Map<String, RepositoryDto>) repositoryMap);
                }
            }
            StorageData storageData = new StorageData(storage);
            return ResponseEntity.ok(storageData);
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, MediaType.APPLICATION_JSON_VALUE);
        }
    }

    @ApiOperation(value = "Deletes a storage.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The storage was removed successfully."),
            @ApiResponse(code = 404, message = "The storage ${storageId} was not found!"),
            @ApiResponse(code = 500, message = "Failed to remove storage ${storageId}!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_DELETE_STORAGE_CONFIGURATION')")
    @DeleteMapping(value = "/{storageId}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity removeStorage(@ApiParam(value = "The storageId", required = true)
                                        @PathVariable final String storageId,
                                        @ApiParam(value = "Whether to force delete and remove the storage from the file system")
                                        @RequestParam(name = "force", defaultValue = "false") final boolean force,
                                        @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            try {
                StorageDto storageDto = getMutableConfigurationClone().getStorage(storageId);
                if (MapUtils.isNotEmpty(storageDto.getRepositories())) {
                    return getFailedResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_STORAGE_REMOVAL_EXISTS_REPOSITORY, accept);
                }
                if (force) {
                    storageManagementService.removeStorage(storageId);
                    repositoryEventListenerRegistry.dispatchRepoDelteAllToCronJobDeleteEvent(storageId, "");
                }
                configurationManagementService.removeStorage(storageId);

                logger.info("Removed storage {}.", storageId);
                SyncStorageDto syncStorageDto = new SyncStorageDto(storageDto, SyncStorageEnum.DELETE, storageId, force);
                clusterSyncService.syncStorage(syncStorageDto);
                AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
                SyncAuthorizationDto syncAuthorizationDto = new SyncAuthorizationDto(authorizationConfigDto, SyncAuthorizationEnum.UPDATE);
                clusterSyncService.syncAuthorization(syncAuthorizationDto);
                return getSuccessfulResponseEntity(SUCCESSFUL_STORAGE_REMOVAL, accept);
            } catch (ConfigurationException | IOException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_STORAGE_REMOVAL, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "Get repository list..")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_REPOSITORY')")
    @GetMapping(value = "/repositories/{storageId}/{repositoryType}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity repositories(@ApiParam(value = "The storageId", required = true)
                                       @PathVariable String storageId,
                                       @ApiParam(value = "The repositoryType", required = true)
                                       @PathVariable
                                               String repositoryType) {
        List<Repository> repositories = configurationManagementService.getRepositoriesWithType(storageId, repositoryType);
        List<RepositoryForm> repositoryForms = Optional.ofNullable(repositories).orElse(Lists.newArrayList()).stream().map(item -> {
            RepositoryForm repository = new RepositoryForm();
            BeanUtils.copyProperties(item, repository);
            repository.setStorageId(item.getStorage().getId());
            if (FileSystemStorageProvider.ALIAS.equalsIgnoreCase(item.getStorageProvider())) {
                repository.setBasedir("/storages/" + repository.getStorageId() + "/" + repository.getId());
            }
            return repository;
        }).collect(Collectors.toList());
        return ResponseEntity.ok(repositoryForms);
    }

    @LicenseAnnotation
    @ApiOperation(value = "Adds or updates a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!"),
            @ApiResponse(code = 500, message = "Failed to remove the repository ${repositoryId}!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity addOrUpdateRepository(@ApiParam(value = "The storageId", required = true)
                                                @PathVariable String storageId,
                                                @ApiParam(value = "The repositoryId", required = true)
                                                @PathVariable
                                                        String repositoryId,
                                                @ApiParam(value = "The repository object", required = true)
                                                @RequestBody
                                                @Validated({Default.class,
                                                        ProxyConfigurationForm.ProxyConfigurationFormChecks.class})
                                                        RepositoryForm repositoryForm,
                                                BindingResult bindingResult,
                                                @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }
            RepositoryDto repository = conversionService.convert(repositoryForm, RepositoryDto.class);
            if (Objects.isNull(repository)) {
                return getFailedResponseEntity(HttpStatus.BAD_REQUEST, "The repository params is null", accept);
            }
            if (repositoryForm.getArtifactMaxSize() == 0) {
                repository.setArtifactMaxSize(107374182400L);
            }
            Repository existRepository = storage.getRepository(repositoryId);
            boolean result = Objects.nonNull(existRepository) && (!repository.getLayout().equals(existRepository.getLayout()) || (Objects.nonNull(existRepository.getSubLayout()) && !existRepository.getSubLayout().equals(repository.getSubLayout())));
            if (result) {
                //判断重复
                return getFailedResponseEntity(HttpStatus.BAD_REQUEST, "The repository id already exists", accept);
            }
            try {
                logger.info("Creating repository {}:{}...", storageId, repositoryId);
                configurationManagementService.saveRepository(storageId, repository);
                RepositoryDto repositoryDto = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(new RepositoryData(repository));
                if (!Files.exists(repositoryPath)) {
                    repositoryManagementService.createRepository(storageId, repositoryId);
                }
                if (Objects.isNull(existRepository) && !RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                    //初始化仓库数据
                    LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repositoryDto.getLayout());
                    layoutProvider.initData(storageId, repositoryId);
                }
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repositoryDto, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);

                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException | RepositoryManagementStrategyException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "Verify if the repository is alive.")
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PostMapping(value = "/{storageId}/{repositoryId}/alive",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<RepositoryAliveForm> aliveRepository(@ApiParam(value = "The storageId", required = true)
                                                               @PathVariable String storageId,
                                                               @ApiParam(value = "The repositoryId", required = true)
                                                               @PathVariable
                                                                       String repositoryId,
                                                               @ApiParam(value = "The repository object", required = true)
                                                               @RequestBody
                                                               @Validated({Default.class,
                                                                       ProxyConfigurationForm.ProxyConfigurationFormChecks.class})
                                                                       RepositoryForm repositoryForm,
                                                               BindingResult bindingResult,
                                                               @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(PARAMS_ERROR, bindingResult);
            }
            RepositoryDto repository = conversionService.convert(repositoryForm, RepositoryDto.class);
            boolean isAlive = false;
            Response response = null;
            int statusCode = 0;
            try {
                if (Objects.nonNull(repository) && Objects.nonNull(repository.getRemoteRepository())) {
                    String repositoryProxyConfigParam = "";
                    if (Objects.nonNull(repository.getProxyConfiguration())) {
                        repositoryProxyConfigParam = JSONObject.toJSONString(repository.getProxyConfiguration());
                    }
                    Client client = clientPool.getRestClient(repositoryProxyConfigParam);
                    //连接建立超时时间
                    client.property(ClientProperties.CONNECT_TIMEOUT, 10000);
                    //读取内容超时时间
                    client.property(ClientProperties.READ_TIMEOUT, 10000);
                    WebTarget target = client.target(repository.getRemoteRepository().getUrl());
                    commonComponent.authentication(target, repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
                    response = target.request().get();
                    statusCode = response.getStatus();
                    isAlive = HttpStatus.OK.value() == statusCode || HttpStatus.MOVED_PERMANENTLY.value() == statusCode ||
                            HttpStatus.FOUND.value() == statusCode;
                    logger.info("Verify if the storage [{}] repository [{}] remoteUrl [{}] is alive responseStatus [{}] response [{}]", storageId, repositoryId, repository.getRemoteRepository().getUrl(), statusCode, response.readEntity(String.class));
                }
                return ResponseEntity.ok(RepositoryAliveForm.builder().alive(isAlive).statusCode(statusCode).build());
            } catch (Exception e) {
                logger.info("Verify if the storage [{}] repository [{}] is alive error [{}]", storageId, repositoryId, ExceptionUtils.getStackTrace(e));
                return ResponseEntity.ok(RepositoryAliveForm.builder().alive(false).build());
            } finally {
                if (Objects.nonNull(response)) {
                    response.close();
                }
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "add repository whites.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/whites",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity addRepositoryWhites(@ApiParam(value = "The storageId", required = true)
                                              @PathVariable String storageId,
                                              @ApiParam(value = "The repositoryId", required = true)
                                              @PathVariable
                                                      String repositoryId,
                                              @ApiParam(value = "The repository object", required = true)
                                              @RequestBody
                                              @Validated({RepositoryForm.WhiteGroup.class})
                                                      RepositoryForm repositoryForm,
                                              BindingResult bindingResult,
                                              @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityWhites(repositoryForm.getVulnerabilityWhites());
                logger.info("新增仓库级别白名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.addRepositoryVulnerabilityWhites(storageId, repositoryId, repository.getVulnerabilityWhites());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "remove repository whites.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}/whites",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity removeRepositoryWhites(@ApiParam(value = "The storageId", required = true)
                                                 @PathVariable String storageId,
                                                 @ApiParam(value = "The repositoryId", required = true)
                                                 @PathVariable
                                                         String repositoryId,
                                                 @ApiParam(value = "The repository object", required = true)
                                                 @RequestBody
                                                 @Validated({RepositoryForm.WhiteGroup.class})
                                                         RepositoryForm repositoryForm,
                                                 BindingResult bindingResult,
                                                 @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityWhites(repositoryForm.getVulnerabilityWhites());
                logger.info("删除仓库级别白名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.removeRepositoryVulnerabilityWhites(storageId, repositoryId, repository.getVulnerabilityWhites());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "add repository blacks.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/blacks",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity addRepositoryBlacks(@ApiParam(value = "The storageId", required = true)
                                              @PathVariable String storageId,
                                              @ApiParam(value = "The repositoryId", required = true)
                                              @PathVariable
                                                      String repositoryId,
                                              @ApiParam(value = "The repository object", required = true)
                                              @RequestBody
                                              @Validated({RepositoryForm.BlackGroup.class})
                                                      RepositoryForm repositoryForm,
                                              BindingResult bindingResult,
                                              @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityBlacks(repositoryForm.getVulnerabilityBlacks());
                logger.info("新增仓库级别黑名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.addRepositoryVulnerabilityBlacks(storageId, repositoryId, repository.getVulnerabilityBlacks());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "remove repository blacks.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}/blacks",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity removeRepositoryBlacks(@ApiParam(value = "The storageId", required = true)
                                                 @PathVariable String storageId,
                                                 @ApiParam(value = "The repositoryId", required = true)
                                                 @PathVariable
                                                         String repositoryId,
                                                 @ApiParam(value = "The repository object", required = true)
                                                 @RequestBody
                                                 @Validated({RepositoryForm.BlackGroup.class})
                                                         RepositoryForm repositoryForm,
                                                 BindingResult bindingResult,
                                                 @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityBlacks(repositoryForm.getVulnerabilityBlacks());
                logger.info("删除仓库级别黑名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.removeRepositoryVulnerabilityBlacks(storageId, repositoryId, repository.getVulnerabilityBlacks());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "set repository whites.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/setWhites",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity setRepositoryWhites(@ApiParam(value = "The storageId", required = true)
                                              @PathVariable String storageId,
                                              @ApiParam(value = "The repositoryId", required = true)
                                              @PathVariable
                                                      String repositoryId,
                                              @ApiParam(value = "The repository object", required = true)
                                              @RequestBody RepositoryForm repositoryForm,
                                              BindingResult bindingResult,
                                              @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityWhites(repositoryForm.getVulnerabilityWhites());
                logger.info("设置仓库级别白名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.setRepositoryVulnerabilityWhites(storageId, repositoryId, repository.getVulnerabilityWhites());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "set repository blacks.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/setBlacks",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity setRepositoryBlacks(@ApiParam(value = "The storageId", required = true)
                                              @PathVariable String storageId,
                                              @ApiParam(value = "The repositoryId", required = true)
                                              @PathVariable
                                                      String repositoryId,
                                              @ApiParam(value = "The repository object", required = true)
                                              @RequestBody RepositoryForm repositoryForm,
                                              BindingResult bindingResult,
                                              @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityBlacks(repositoryForm.getVulnerabilityBlacks());
                logger.info("设置仓库级别黑名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.setRepositoryVulnerabilityBlacks(storageId, repositoryId, repository.getVulnerabilityBlacks());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "Returns the configuration of a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully.", response = RepositoryDto.class),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_REPOSITORY')")
    @GetMapping(value = "/{storageId}/{repositoryId}",
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getRepositoryResponseEntity(@RepositoryMapping(allowOutOfServiceRepository = true) Repository repository) {
        if (repository.isGroupRepository()) {
            repository = configurationManagementService.getMutableConfigurationClone().getStorage(repository.getStorage().getId()).getRepository(repository.getId());
            Set<String> vulnerabilityWhites = repository.getVulnerabilityWhites(), vulnerabilityBlacks = repository.getVulnerabilityBlacks();
            List<String> storageAndRepositoryIdList = Lists.newArrayList();
            configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
            for (String storageAndRepositoryId : storageAndRepositoryIdList) {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManagementService.getMutableConfigurationClone().getStorage(sId).getRepository(rId);
                vulnerabilityWhites.addAll(subRepository.getVulnerabilityWhites());
                vulnerabilityBlacks.addAll(subRepository.getVulnerabilityBlacks());
            }
        }
        return ResponseEntity.ok(repository);
    }

    @ApiOperation(value = "Deletes a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was deleted successfully."),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!"),
            @ApiResponse(code = 500, message = "Failed to remove the repository ${repositoryId}!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_DELETE_REPOSITORY')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}",
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity removeRepository(@RepositoryMapping(allowOutOfServiceRepository = true) Repository repository,
                                           @ApiParam(value = "Whether to force delete the repository from the file system")
                                           @RequestParam(name = "force", defaultValue = "false") final boolean force,
                                           @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        try {
            List<Repository> repositoryList = configurationManagementService.getConfiguration().getGroupRepositoriesContaining(storageId , repositoryId);
            if (CollectionUtils.isNotEmpty(repositoryList)) {
                return getFailedResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, String.format(FAILED_REPOSITORY_REMOVAL_EXISTS_GROUP_REPOSITORY, repositoryList.stream().map(item -> String.format("%s:%s", item.getStorage().getId(), item.getId())).collect(Collectors.joining(","))), accept);
            }
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
            RepositoryDto repositoryDto = getMutableConfigurationClone().getStorage(storageId)
                    .getRepository(repositoryId);
            if (force && Files.exists(repositoryPath)) {
                repositoryManagementService.removeRepository(storageId, repository.getId());
                repositoryEventListenerRegistry.dispatchRepoDelteToCronJobDeleteEvent(storageId, repositoryId);
            }

            configurationManagementService.removeRepository(storageId, repositoryId);
            SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repositoryDto, storageId, repositoryId, SyncRepositoryEnum.DELETE, force);
            clusterSyncService.syncRepository(syncRepositoryDto);

            logger.info("Removed repository {}:{}.", storageId, repositoryId);

            return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_REMOVAL, accept);
        } catch (IOException | ConfigurationException e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_REMOVAL, e, accept);
        }
    }

    @ApiOperation(value = "set repository permissions.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "ok."),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PostMapping(value = "/{storageId}/{repositoryId}/permission",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity repositoryPermission(@ApiParam(value = "The storageId", required = true)
                                               @PathVariable String storageId,
                                               @ApiParam(value = "The repositoryId", required = true)
                                               @PathVariable
                                                       String repositoryId,
                                               @ApiParam(value = "The repository object", required = true)
                                               @RequestBody
                                               @Validated RepositoryPermissionForm repositoryPermissionForm,
                                               BindingResult bindingResult,
                                               @RequestHeader(HttpHeaders.ACCEPT) String accept) throws IOException {
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY_PERMISSION, bindingResult);
            }
            RepositoryPermissionDto repositoryPermissionDto = conversionService.convert(repositoryPermissionForm, RepositoryPermissionDto.class);
            if (Objects.isNull(repositoryPermissionDto)) {
                return getFailedResponseEntity(HttpStatus.BAD_REQUEST, FAILED_SAVE_REPOSITORY_PERMISSION, accept);
            }
            if (RepositoryScopeEnum.STORAGE.getType().equals(repositoryPermissionDto.getScope())) {
                if (CollectionUtils.isNotEmpty(storage.getUsers())) {
                    //存储空间内，但是参数中包含了其他成员
                    List<RepositoryPermissionUserDto> userList = Optional.ofNullable(repositoryPermissionDto.getUserList()).orElse(Collections.emptyList()).stream().filter(item -> !storage.getUsers().contains(item.getUsername())).collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(userList)) {
                        String uses = userList.stream().map(RepositoryPermissionUserDto::getUsername).collect(Collectors.joining(","));
                        return getFailedResponseEntity(HttpStatus.BAD_REQUEST, String.format(FAILED_SAVE_REPOSITORY_PERMISSION_USER, uses), accept);
                    }
                } else if (CollectionUtils.isNotEmpty(repositoryPermissionDto.getUserList())) {
                    String uses = repositoryPermissionDto.getUserList().stream().map(RepositoryPermissionUserDto::getUsername).collect(Collectors.joining(","));
                    return getFailedResponseEntity(HttpStatus.BAD_REQUEST, String.format(FAILED_SAVE_REPOSITORY_PERMISSION_USER, uses), accept);
                }
            }
            RepositoryDto repository = configurationManagementService.getMutableConfigurationClone().getStorage(storageId).getRepository(repositoryId);
            repository.setScope(repositoryPermissionDto.getScope());
            repository.setAllowAnonymous(repositoryPermissionDto.isAllowAnonymous());
            configurationManagementService.saveRepository(storageId, repository);
            SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
            clusterSyncService.syncRepository(syncRepositoryDto);
            repositoryManagementService.handlerRepositoryPermission(storageId, repositoryId, repositoryPermissionDto);
            AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
            SyncAuthorizationDto syncAuthorizationDto = new SyncAuthorizationDto(authorizationConfigDto, SyncAuthorizationEnum.UPDATE);
            clusterSyncService.syncAuthorization(syncAuthorizationDto);
            return ResponseEntity.ok("ok");
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "get repository enable users.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "ok."),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/repositoryEnableUsers")
    public ResponseEntity repositoryEnableUsers(@ApiParam(value = "The storageId", required = true)
                                                @RequestParam String storageId,
                                                @ApiParam(value = "The repositoryId", required = true)
                                                @RequestParam String repositoryId, @ApiParam(value = "The scope") @RequestParam(name = "scope", required = false) Integer scope,
                                                @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            List<String> usernameList = null;
            Repository repository = storage.getRepository(repositoryId);
            if ((RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()) && !RepositoryScopeEnum.STORAGE.getType().equals(scope)) || RepositoryScopeEnum.OPEN.getType().equals(scope)) {
                //公开仓库，除了ADMIN、ARTIFACTS_MANAGER角色和存储空间管理员的其他所有人
                Users users = userService.getUsers();
                if (Objects.nonNull(users) && CollectionUtils.isNotEmpty(users.getUsers())) {
                    //排除有管理员、ARTIFACTS_MANAGER角色的用户
                    List<User> usersList = users.getUsers().stream().filter(item -> CollectionUtils.isNotEmpty(item.getRoles()) && item.getRoles().stream().noneMatch(role -> SystemRole.ADMIN.name().equals(role.getRoleName()) || SystemRole.ARTIFACTS_MANAGER.name().equals(role.getRoleName()))).collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(usersList)) {
                        //排除存储空间管理员
                        usernameList = usersList.stream().filter(item -> !item.getUsername().equals(storage.getAdmin())).map(User::getUsername).collect(Collectors.toList());
                    }
                }
            } else {
                //存储空间下仓库，返回仓库成员
                if (CollectionUtils.isNotEmpty(storage.getUsers())) {
                    User user;
                    boolean flag;
                    usernameList = Lists.newArrayList();
                    for (String username : storage.getUsers()) {
                        if (username.equals(storage.getAdmin())) {
                            //过滤存储空间管理员
                            continue;
                        }
                        user = userService.findByUsername(username);
                        //过滤管理员、ARTIFACTS_MANAGER角色的用户
                        flag = Objects.nonNull(user) && CollectionUtils.isNotEmpty(user.getRoles()) && user.getRoles().stream().noneMatch(role -> SystemRole.ADMIN.name().equals(role.getRoleName()) || SystemRole.ARTIFACTS_MANAGER.name().equals(role.getRoleName()));
                        if (!flag) {
                            continue;
                        }
                        usernameList.add(user.getUsername());
                    }
                }
            }
            return ResponseEntity.ok(usernameList);
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "get repository permission users.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "ok."),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/repositoryPermission")
    public ResponseEntity repositoryPermission(@ApiParam(value = "The storageId", required = true)
                                               @RequestParam String storageId,
                                               @ApiParam(value = "The repositoryId", required = true)
                                               @RequestParam
                                                       String repositoryId,
                                               @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            Repository repository = storage.getRepository(repositoryId);
            String repositoryDeployRoleName = String.format("%s|%s|%s", storageId.toUpperCase(), repositoryId.toUpperCase(), Privileges.ARTIFACTS_DEPLOY.getAuthority());
            String repositoryDeleteRoleName = String.format("%s|%s|%s", storageId.toUpperCase(), repositoryId.toUpperCase(), Privileges.ARTIFACTS_DELETE.getAuthority());
            List<String> roleNameList = Lists.newArrayList(repositoryDeployRoleName, repositoryDeleteRoleName);
            List<String> excludeRoleNameList = Lists.newArrayList(SystemRole.ARTIFACTS_MANAGER.name(), SystemRole.ADMIN.name());
            Users userList = userService.getUsers();
            roleNameList.addAll(userList.getUsers().stream().filter(user -> user.getRoles().stream().noneMatch(r -> excludeRoleNameList.contains(r.getRoleName()))).map(user -> user.getUsername().toUpperCase()).collect(Collectors.toList()));
            List<User> users = userService.findUserByRoles(roleNameList);
            AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
            RepositoryPermission repositoryPermission = RepositoryPermission.builder().build();
            List<String> permissionList = Lists.newArrayList(Privileges.ARTIFACTS_DEPLOY.getAuthority(), Privileges.ARTIFACTS_DELETE.getAuthority());
            List<RepositoryUser> repositoryUserList = Optional.ofNullable(users).orElse(Collections.emptyList()).stream().map(user -> {
                RepositoryUser repositoryUser = RepositoryUser.builder().build();
                repositoryUser.setUsername(user.getUsername());
                List<String> permissions = Lists.newArrayList(), paths = Lists.newArrayList(), roleNames = user.getRoles().stream().map(SecurityRole::getRoleName).collect(Collectors.toList());
                boolean deployFlag = user.getRoles().stream().anyMatch(role -> repositoryDeployRoleName.equals(role.getRoleName()));
                if (deployFlag) {
                    permissions.add(Privileges.ARTIFACTS_DEPLOY.getAuthority());
                }
                if (user.getRoles().stream().anyMatch(role -> repositoryDeleteRoleName.equals(role.getRoleName()))) {
                    permissions.add(Privileges.ARTIFACTS_DELETE.getAuthority());
                }
                paths = authorizationConfigDto.getRoles().stream().filter(auth -> roleNames.contains(auth.getName())).flatMap(item -> item.getAccessModel().getStorageAuthorities().stream().filter(s -> s.getStorageId().equals(storageId)))
                        .flatMap(item -> item.getRepositoryPrivileges().stream().filter(r -> r.getRepositoryId().equals(repositoryId)))
                        .flatMap(item -> item.getPathPrivileges().stream().filter(i -> StringUtils.isNotBlank(i.getPath())))
                        .map(PathPrivilegesDto::getPath).distinct()
                        .collect(Collectors.toList());
                permissions.addAll(authorizationConfigDto.getRoles().stream().filter(auth -> roleNames.contains(auth.getName())).flatMap(item -> item.getAccessModel().getStorageAuthorities().stream().filter(s -> s.getStorageId().equals(storageId)))
                        .flatMap(item -> item.getRepositoryPrivileges().stream().filter(r -> r.getRepositoryId().equals(repositoryId)))
                        .flatMap(item -> item.getPathPrivileges().stream().filter(i -> StringUtils.isNotBlank(i.getPath())))
                        .flatMap(item -> item.getPrivileges().stream())
                        .filter(p -> permissionList.contains(p.toString())).map(Enum::toString).distinct()
                        .collect(Collectors.toList()));
                repositoryUser.setPaths(String.join(",", paths));
                repositoryUser.setPermissions(permissions);
                return repositoryUser;
            }).filter(repositoryUser -> CollectionUtils.isNotEmpty(repositoryUser.getPermissions())).collect(Collectors.toList());
            repositoryPermission.setScope(repository.getScope());
            repositoryPermission.setAllowAnonymous(repository.isAllowAnonymous());
            repositoryPermission.setUserList(repositoryUserList);
            return ResponseEntity.ok(repositoryPermission);
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "delete users repository permission.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "ok."),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @DeleteMapping(value = "/repositoryPermission")
    public ResponseEntity deletePermission(@ApiParam(value = "The storageId", required = true)
                                           @RequestParam String storageId,
                                           @ApiParam(value = "The repositoryId", required = true)
                                           @RequestParam String repositoryId,
                                           @ApiParam(value = "The username", required = true)
                                           @RequestParam String username,
                                           @ApiParam(value = "The permissions", required = true)
                                           @RequestParam String permissions,
                                           @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            repositoryManagementService.deleteRepositoryPermission(storageId, repositoryId, username, permissions);
            return ResponseEntity.ok("ok");
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "set union repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/unionRepository",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity unionRepository(@ApiParam(value = "The storageId", required = true)
                                          @PathVariable String storageId,
                                          @ApiParam(value = "The repositoryId", required = true)
                                          @PathVariable
                                                  String repositoryId,
                                          @ApiParam(value = "The repository object", required = true)
                                          @RequestBody
                                          @Validated
                                                  UnionRepositoryConfigurationForm unionRepositoryConfigurationForm,
                                          BindingResult bindingResult,
                                          @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }
            try {
                Repository repository = getConfiguration().getRepository(storageId, repositoryId);
                if (Objects.isNull(repository)) {
                    return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
                }
                logger.info("设置仓库的联邦仓库 {}:{}...", storageId, repositoryId);
                configurationManagementService.setUnionRepositoryConfiguration(storageId, repositoryId, unionRepositoryConfigurationForm.getMutableUnionRepositoryConfiguration());
                //同步联邦仓库
                SyncUnionRepositoryDto syncUnionRepositoryDto = SyncUnionRepositoryDto.builder().storageId(storageId).repositoryId(repositoryId)
                        .syncUnionRepositoryEnum(SyncUnionRepositoryEnum.ADD_OR_UPDATE).unionRepositoryConfigurationForm(unionRepositoryConfigurationForm).build();
                clusterSyncService.syncUnionRepositoryConfiguration(syncUnionRepositoryDto);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }
}

