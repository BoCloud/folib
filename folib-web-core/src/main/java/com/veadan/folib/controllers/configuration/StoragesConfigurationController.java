package com.veadan.folib.controllers.configuration;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.json.JSONUtil;
import com.fasterxml.jackson.annotation.JsonView;
import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.cluster.SyncStorageEnum;
import com.veadan.folib.config.PermissionCheck;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.controllers.cluster.dto.SyncStorageDto;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.DispatchStorageTree;
import com.veadan.folib.dto.ArtifactDispatchRepositoryDto;
import com.veadan.folib.event.repository.RepositoryEventListenerRegistry;
import com.veadan.folib.forms.common.StorageTreeForm;
import com.veadan.folib.forms.configuration.ProxyConfigurationForm;
import com.veadan.folib.forms.configuration.RepositoryForm;
import com.veadan.folib.forms.configuration.StorageForm;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.storage.FileSystemStorageProvider;
import com.veadan.folib.repository.RepositoryManagementStrategyException;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.services.support.ConfigurationException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.Views;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.validation.RequestBodyValidationException;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.*;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
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

import javax.validation.groups.Default;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.nio.file.Files;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
@RestController
@RequestMapping("/api/configuration/folib/storages")
@Api(value = "/api/configuration/folib/storages")
public class StoragesConfigurationController
        extends BaseConfigurationController {
    static final String SUCCESSFUL_SAVE_STORAGE = "The storage was created successfully.";

    static final String FAILED_SAVE_STORAGE_FORM_ERROR = "The storage cannot be created because the submitted form contains errors!";

    static final String FAILED_SAVE_STORAGE_ERROR = "The storage was not created.";

    static final String SUCCESSFUL_UPDATE_STORAGE = "The storage was updated successfully.";

    static final String FAILED_UPDATE_STORAGE_FORM_ERROR = "The storage cannot be updated because the submitted form contains errors!";

    static final String FAILED_UPDATE_STORAGE_ERROR = "The storage was not updated.";

    static final String FAILED_SAVE_REPOSITORY = "The repository cannot be saved because the submitted form contains errors!";

    static final String SUCCESSFUL_REPOSITORY_SAVE = "The repository was updated successfully.";

    static final String FAILED_REPOSITORY_SAVE = "The repository was not saved.";

    static final String SUCCESSFUL_STORAGE_REMOVAL = "The storage was removed successfully.";

    static final String SUCCESSFUL_REPOSITORY_REMOVAL = "The repository was removed successfully.";

    private static final String FAILED_STORAGE_REMOVAL = "Failed to remove the storage !";

    private static final String STORAGE_NOT_FOUND = "The storage was not found.";

    private static final String FAILED_REPOSITORY_REMOVAL = "Failed to remove the repository !";

    private final StorageManagementService storageManagementService;

    private final RepositoryManagementService repositoryManagementService;

    private final ConversionService conversionService;

    @Autowired
    private ClusterSyncService clusterSyncService;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private RepositoryEventListenerRegistry repositoryEventListenerRegistry;

    public StoragesConfigurationController(ConfigurationManagementService configurationManagementService,
                                           StorageManagementService storageManagementService,
                                           RepositoryManagementService repositoryManagementService,
                                           ConversionService conversionService) {
        super(configurationManagementService);
        this.storageManagementService = storageManagementService;
        this.repositoryManagementService = repositoryManagementService;
        this.conversionService = conversionService;
    }

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
            storageManagementService.createStorage(storage);
            // 向其他集群节点同步storage
            SyncStorageDto syncStorageDto = new SyncStorageDto(storage, storageForm.getId(), SyncStorageEnum.CREATE);
            clusterSyncService.syncStorage(syncStorageDto);

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
            storageManagementService.updateStorage(storage);
            SyncStorageDto syncStorageDto = new SyncStorageDto(storage, storageId, SyncStorageEnum.UPDATE);
            clusterSyncService.syncStorage(syncStorageDto);
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
        Set<String> roleNames = roleNames(authentication);
        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        StoragesOutput storagesOutput = new StoragesOutput(storages);
        if (!roleNames.contains(SystemRole.ADMIN.name())) {
            List<Storage> list = storagesOutput.getStorages();
            List<Storage> collect = list.stream().filter(s -> (CollectionUtil.isNotEmpty(s.getUsers()) && s.getUsers().contains(loggedUser.getUsername()))).collect(Collectors.toList());
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
        List<StorageTreeForm> storageTreeForms = Lists.newArrayList();
        if (CollectionUtil.isNotEmpty(storages)) {
            Set<String> roleNames = roleNames(authentication);
            boolean filterByUser = !roleNames.contains(SystemRole.ADMIN.name());
            boolean filterByStorageId = StringUtils.isNotBlank(storageId);
            boolean filterByType = StringUtils.isNotBlank(type);
            boolean filterByLayout = StringUtils.isNotBlank(layout);
            boolean filterByExcludeRepositoryId = StringUtils.isNotBlank(excludeRepositoryId);
            boolean filterByPolicy = StringUtils.isNotBlank(policy);
            storages = storages.stream()
                    .distinct()
                    .filter(s -> !filterByUser || (s.getUsers() != null && s.getUsers().contains(loggedUser.getUsername())))
                    .filter(s -> !filterByStorageId || s.getId().equalsIgnoreCase(storageId))
                    .collect(Collectors.toCollection(LinkedList::new));
            StorageTreeForm storageTreeForm;
            List<Repository> repositories;
            for (Storage storage : storages) {
                storageTreeForm = StorageTreeForm.builder().id(storage.getId()).key(storage.getId()).name(storage.getId()).build();
                repositories = new LinkedList<Repository>(storage.getRepositories().values());
                repositories = repositories.stream().distinct()
                        .filter(r -> !filterByType || r.getType().equalsIgnoreCase(type))
                        .filter(r -> !filterByLayout || r.getLayout().equalsIgnoreCase(layout))
                        .filter(r -> !filterByPolicy || r.getPolicy().equalsIgnoreCase(policy))
                        .filter(r -> !filterByExcludeRepositoryId || !r.getId().equalsIgnoreCase(excludeRepositoryId))
                        .collect(Collectors.toCollection(LinkedList::new));
                storageTreeForm.setChildren(repositories.stream().map(repository -> StorageTreeForm.builder().id(repository.getId()).key(storage.getId() + "," + repository.getId()).name(repository.getId()).type(repository.getType()).layout(repository.getLayout()).build()).collect(Collectors.toList()));
                storageTreeForms.add(storageTreeForm);
            }
        }
        return ResponseEntity.ok(storageTreeForms);
    }

    @PostMapping(value = "/getDispatchRepositories", produces = MediaType.APPLICATION_JSON_VALUE)
    @PermissionCheck(resourceKey = "ARTIFACTS_VIEW")
    public ResponseEntity getDispatchRepositories(@RequestBody ArtifactDispatchRepositoryDto dispatchRepositoryDto) {
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
                .name(dispatchEnName).build();
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
                        .name(storage.getId()).build();
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
        Client client = clientPool.getRestClient();
        ArtifactDispatchRepositoryDto dispatchRepositoryDto = ArtifactDispatchRepositoryDto.builder()
                .type(type)
                .layout(layout)
                .policy(policy).build();
        List<StorageTreeForm> repoList = new LinkedList<>();
        for (ClusterDispatchNodeDto clusterDispatchNodeDto : listDispatch) {
            String dispatchEnName = clusterDispatchNodeDto.getClusterEnName();
            try {
                String host = clusterDispatchNodeDto.getClusterNodeHost();
                String url = host.endsWith("/") ? host + "api/configuration/folib/storages/getDispatchRepositories" :
                        host + "/api/configuration/folib/storages/getDispatchRepositories";
                dispatchRepositoryDto.setDispatchEnName(dispatchEnName);
                WebTarget target = client.target(url);
                logger.info(" 请求分发获取仓库信息 {}", JSONUtil.toJsonStr(dispatchRepositoryDto));
                Response response = target.request().post(Entity.entity(dispatchRepositoryDto, javax.ws.rs.core.MediaType.APPLICATION_JSON));
                if (response.getStatus() != 200) {
                    logger.error("dispatch cluster {} get repositroy fail", dispatchEnName);
                    continue;
                }
                DispatchStorageTree dispatchStorageTree = response.readEntity(DispatchStorageTree.class);
                repoList.addAll(dispatchStorageTree.getList());
            } catch (Exception e) {
                logger.error("分发获取 {} 仓库信息失败! {}", dispatchEnName, e.getMessage());
            }
        }
        // 发送获取仓库信息Task
        return ResponseEntity.ok(repoList);
    }



    @JsonView(Views.LongStorage.class)
    @ApiOperation(value = "Retrieve the configuration of a storage.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "The storage ${storageId} was not found.")})
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_STORAGE_CONFIGURATION')")
    @GetMapping(value = "/{storageId}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getStorageResponseEntity(@ApiParam(value = "The storageId", required = true)
                                                   @PathVariable final String storageId) {
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);

        if (storage != null) {
            return ResponseEntity.ok(storage);
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
                if (force) {
                    storageManagementService.removeStorage(storageId);
                    repositoryEventListenerRegistry.dispatchRepoDelteAllToCronJobDeleteEvent(storageId, "");
                }
                configurationManagementService.removeStorage(storageId);

                logger.debug("Removed storage {}.", storageId);
                SyncStorageDto syncStorageDto = new SyncStorageDto(storageDto, SyncStorageEnum.DELETE, storageId, force);
                clusterSyncService.syncStorage(syncStorageDto);

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
            if (repositoryForm.getArtifactMaxSize() == 0) {
                assert repository != null;
                repository.setArtifactMaxSize(214748364800L);
            }
            Repository existRepository = storage.getRepository(repositoryId);
            boolean result = Objects.nonNull(existRepository) && Objects.nonNull(repository) && (!repository.getLayout().equals(existRepository.getLayout()) || (Objects.nonNull(existRepository.getSubLayout()) && !existRepository.getSubLayout().equals(repository.getSubLayout())));
            if (result) {
                //判断重复
                return getFailedResponseEntity(HttpStatus.BAD_REQUEST, "The repository id already exists", accept);
            }
            try {
                logger.debug("Creating repository {}:{}...", storageId, repositoryId);

                configurationManagementService.saveRepository(storageId, repository);
                RepositoryDto repositoryDto = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);

                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(new RepositoryData(repository));
                if (!Files.exists(repositoryPath)) {
                    repositoryManagementService.createRepository(storageId, repository.getId());
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

    @ApiOperation(value = "add repository whites.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/whites/{storageId}/{repositoryId}",
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
                logger.debug("新增仓库级别白名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.addRepositoryVulnerabilityWhites(storageId, repositoryId, repository.getVulnerabilityWhites());
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
    @DeleteMapping(value = "/whites/{storageId}/{repositoryId}",
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
                logger.debug("删除仓库级别白名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.removeRepositoryVulnerabilityWhites(storageId, repositoryId, repository.getVulnerabilityWhites());
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, SyncRepositoryEnum.DELETE);
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
    @PutMapping(value = "/blacks/{storageId}/{repositoryId}",
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
                logger.debug("新增仓库级别黑名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.addRepositoryVulnerabilityBlacks(storageId, repositoryId, repository.getVulnerabilityBlacks());
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
    @DeleteMapping(value = "/blacks/{storageId}/{repositoryId}",
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
                logger.debug("删除仓库级别黑名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.removeRepositoryVulnerabilityBlacks(storageId, repositoryId, repository.getVulnerabilityBlacks());
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, SyncRepositoryEnum.DELETE);
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
    @PutMapping(value = "/setWhites/{storageId}/{repositoryId}",
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
                logger.debug("设置仓库级别白名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.setRepositoryVulnerabilityWhites(storageId, repositoryId, repository.getVulnerabilityWhites());
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
    @PutMapping(value = "/setBlacks/{storageId}/{repositoryId}",
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
                logger.debug("设置仓库级别黑名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.setRepositoryVulnerabilityBlacks(storageId, repositoryId, repository.getVulnerabilityBlacks());
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
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
            RepositoryDto repositoryDto = getMutableConfigurationClone().getStorage(storageId)
                    .getRepository(repositoryId);
            if (Files.exists(repositoryPath) && force) {
                repositoryManagementService.removeRepository(storageId, repository.getId());
                repositoryEventListenerRegistry.dispatchRepoDelteToCronJobDeleteEvent(storageId, repositoryId);
            }

            configurationManagementService.removeRepository(storageId, repositoryId);
            SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repositoryDto, storageId, repositoryId, SyncRepositoryEnum.DELETE,force);
            clusterSyncService.syncRepository(syncRepositoryDto);

            logger.debug("Removed repository {}:{}.", storageId, repositoryId);

            return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_REMOVAL, accept);
        } catch (IOException | ConfigurationException e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_REMOVAL, e, accept);
        }
    }

}
