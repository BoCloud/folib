package com.veadan.folib.controllers.unicom;

import com.alibaba.fastjson.JSON;
import com.google.common.collect.Lists;
import com.veadan.folib.components.CostumeSecurityAdapter;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.enums.RepositoryScopeEnum;
import com.veadan.folib.forms.common.StorageTreeForm;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageData;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import javax.annotation.Resource;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author huayanjun
 * @since 2024-12-10 10:23
 */
@Slf4j
@Component
public class UnicomAdapter implements CostumeSecurityAdapter {

    public static HashSet<String> adminRole;

    static {
        adminRole = new HashSet<>();
        adminRole.add("研发组长");
        adminRole.add("QA");
    }

    public static final String UNICOM_SOURCE_ID = "unicomUserDetailService";

    @Resource
    private ConfigurationManagementService configurationManagementService;

    @Resource
    private ConfigurationManager configurationManager;

    @Resource
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;


    @Resource
    private RestTemplate restTemplate;

    @Resource
    private UnicomConfig unicomConfig;


    public UnicomRoleDTO getUserDetail(String userEmail) {
        try {
            String url = unicomConfig.getUserDetailUrl();
            HttpHeaders header = getHeader();
            header.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
            header.setAccept(Collections.singletonList(MediaType.APPLICATION_JSON));
            MultiValueMap<String, Object> params = new LinkedMultiValueMap<>();
            List<String> emails = Collections.singletonList(userEmail);
            params.add("json", emails);
            HttpEntity<MultiValueMap<String, Object>> entity = new HttpEntity<>(params, header);
            log.info("发送用户信息地址是{},发送内容为{}", url, JSON.toJSONString(entity));
            ResponseEntity<String> response = restTemplate.exchange(url, HttpMethod.POST, entity, String.class);
            if (response.getStatusCode() == HttpStatus.OK) {
                UnicomRoleDTO unicomRoleDTO = JSON.parseObject(response.getBody(),UnicomRoleDTO.class);
                log.debug("get user: {} detail success", userEmail);
                if (response.getBody() == null || unicomRoleDTO.getCode() != 200) {
                    log.error("根据用户邮箱获取信息失败");
                    return null;
                }
                if (unicomRoleDTO.getData().size() != 1) {
                    log.error("根据用户获取邮箱信息唯一");
                    return null;
                }
                return unicomRoleDTO;
            } else {
                return null;
            }
        } catch (Exception e) {
            log.debug("get user: {} detail error:{}", userEmail, e.getMessage(), e);
            return null;
        }
    }

    public UicomUserDTO verify(String sessionId) {
        try {
            HttpHeaders header = getHeader();
            header.setAccept(Collections.singletonList(MediaType.APPLICATION_JSON));
            // 这里需要将sessionId放到query参数中，
            String url = unicomConfig.getVerifyUrl() + "?sessionId=" + sessionId;
            HttpEntity<String> entity = new HttpEntity<>(header);
            log.info("发送验证地址是{},发送内容为{}", url, JSON.toJSONString(entity));
            ResponseEntity<String> response = restTemplate.exchange(url, HttpMethod.POST, entity, String.class);

            if (response.getStatusCode() == HttpStatus.OK) {
                log.info("联通身份认证 verify success,sessionId:{},userDetail{}", sessionId, response.getBody());
                UicomUserDTO userDTO = JSON.parseObject(response.getBody(), UicomUserDTO.class);
                if("200".equals(userDTO.getCode())){
                    userDTO.fullFiled();
                }else {
                    return null;
                }
            }
            return null;
        } catch (Exception e) {
            log.error("verify failed,sessionId:{},error:{}", sessionId, e.getMessage(), e);
            return null;
        }
    }

    public void sendEmail(UnicomEmailDTO emailDTO) {
        try {
            HttpHeaders header = getHeader();
            header.setContentType(MediaType.APPLICATION_JSON);
            header.setAccept(Collections.singletonList(MediaType.APPLICATION_JSON));
            String url = unicomConfig.getSendEmailUrl();
            HttpEntity<String> entity = new HttpEntity<>(JSON.toJSONString(emailDTO), header);
            log.info("发送邮件地址是{},发送内容为{}", url, JSON.toJSONString(entity));
            ResponseEntity<UnicomCommonResponse> response = restTemplate.exchange(url, HttpMethod.POST, entity, UnicomCommonResponse.class);
            if (response.getStatusCode().is2xxSuccessful()) {
                if (200 == response.getBody().getCode()) {
                    log.info("邮件发送成功!{}", response.getBody().getMessage());
                } else {
                    log.error("邮件发送失败{}", response.getBody().getMessage());
                }
            } else {
                log.error("邮件发送失败");
            }
        } catch (Exception e) {
            log.error("发送邮件接口异常,{}", e.getMessage(), e);
        }
    }

    public UicomUserDTO getUserDetailByName(String loginName) {
        try {
            HttpHeaders header = getHeader();
            header.setContentType(MediaType.APPLICATION_JSON);
            header.setAccept(Collections.singletonList(MediaType.APPLICATION_JSON));
            String url = unicomConfig.getUserInfoUrl();
            HashMap<Object, Object> data = new HashMap<>();
            data.put("loginName", loginName);
            HttpEntity<HashMap<Object, Object>> entity = new HttpEntity<>(data, header);
            log.info("发送用户信息{},发送内容为{}", url, JSON.toJSONString(entity));
            ResponseEntity<String> response = restTemplate.exchange(url, HttpMethod.POST, entity, String.class);
            if (response.getStatusCode().is2xxSuccessful()) {
                log.info("获取用户信息返回{}",response.getBody());
                UicomUserDetail detail = JSON.parseObject(response.getBody(), UicomUserDetail.class);
                if ("200".equals(detail.getCode())) {
                    log.info("获取用户信息{}成功",loginName);
                    UicomUserDTO userDTO = new UicomUserDTO();
                    userDTO.setLoginName(loginName);
                    userDTO.setEmail(detail.getData().getEmail());
                    return userDTO;
                } else {
                    return null;
                }
            } else {
                log.error("获取用户信息失败");
                return null;
            }
        }catch (Exception e) {
            return null;
        }
    }

    public void sendMessageEmail(String title, String message, String mail) {
        UnicomEmailDTO emailDTO = new UnicomEmailDTO();
        emailDTO.setTitle(title);
        emailDTO.setAccount(mail);
        emailDTO.setContent(message);
        sendEmail(emailDTO);
    }


    public HttpHeaders getHeader() {
        String timeStamp = String.valueOf(System.currentTimeMillis());
        String appCode = unicomConfig.getAppCode();
        String appSecret = unicomConfig.getAppSecret();
        HttpHeaders header = new HttpHeaders();
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            md.update(appCode.getBytes(Charset.defaultCharset()));
            md.update(appSecret.getBytes(Charset.defaultCharset()));
            md.update(timeStamp.getBytes(Charset.defaultCharset()));
            byte[] result = md.digest();
            String signature = new BigInteger(1, result).toString(16);
            log.info("Sha256Gen,apCode:{},appSecret:{},timeStamp:{},signature:{}", appCode, appSecret, timeStamp, signature);
            header.add("appId", unicomConfig.getAppCode());
            header.add("signature", signature);
            header.add("timestamp", timeStamp);
        } catch (Exception e) {
            log.error("Sha256Gen failed:{}", e.getMessage(), e);
        }
        return header;
    }

    public TableResultResponse<Repository> getStoragesAndRepositories(String storageId, String name, String type, String excludeType, String excludeRepositoryId, String layout,
                                                                      String policy, Authentication authentication, Integer page, Integer limit) {
        List<Storage> storages = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());

        List<Repository> repositorieList = new ArrayList<>();
        List<StorageTreeForm> storageTreeForms = Lists.newArrayList();
        //  获取当前用户
        SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
        UnicomRoleDTO unicomRoleDTO = getUserDetail(userDetails.getEmail());
        Set<String> projects;
        if (unicomRoleDTO != null) {
            projects = unicomRoleDTO.ownProject();
        } else {
            projects = new HashSet<>();
        }
        boolean filterByStorageId = StringUtils.isNotBlank(storageId);
        boolean filterByType = StringUtils.isNotBlank(type);
        boolean filterByLayout = StringUtils.isNotBlank(layout);
        boolean filterByExcludeRepositoryId = StringUtils.isNotBlank(excludeRepositoryId);
        boolean filterByExcludeType = StringUtils.isNotBlank(excludeType);
        boolean filterByPolicy = StringUtils.isNotBlank(policy);
        boolean filterByName = StringUtils.isNotBlank(name);
        String excludedStorageId = "", excludedRepositoryId = "";
        if (filterByExcludeRepositoryId) {
            excludedStorageId = ConfigurationUtils.getStorageId(storageId, excludeRepositoryId);
            excludedRepositoryId = ConfigurationUtils.getRepositoryId(excludeRepositoryId);
        }
        String excludedStorageIdAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(excludedStorageId, excludedRepositoryId);
        storages = storages.stream()
                .distinct()
                .filter(s -> !filterByStorageId || s.getId().equalsIgnoreCase(storageId))
                .collect(Collectors.toCollection(LinkedList::new));
        StorageTreeForm storageTreeForm;
        List<Repository> repositories;

        for (Storage storage : storages) {
            storageTreeForm = StorageTreeForm.builder().id(storage.getId()).key(storage.getId()).name(storage.getId()).build();
            repositories = new LinkedList<>(storage.getRepositories().values());
            repositories = repositories.stream().distinct()
                    .filter(r -> !filterByType || r.getType().equalsIgnoreCase(type))
                    .filter(r -> !filterByLayout || r.getLayout().equalsIgnoreCase(layout))
                    .filter(r -> !filterByPolicy || r.getPolicy().equalsIgnoreCase(policy))
                    .filter(r -> !filterByExcludeRepositoryId || (!r.getStorageIdAndRepositoryId().equalsIgnoreCase(excludedStorageIdAndRepositoryId)))
                    .filter(r -> !filterByExcludeType || !r.getType().equalsIgnoreCase(excludeType))
                    .filter(r -> !filterByName || r.getId().toLowerCase().contains(name.toLowerCase()))
                    .filter(r -> projects.contains(r.getProjectId()))
                    .collect(Collectors.toCollection(LinkedList::new));
            if (!repositories.isEmpty()) {
                repositorieList.addAll(repositories);
                storageTreeForm.setChildren(repositories.stream().map(repository -> StorageTreeForm.builder().id(repository.getId()).key(storage.getId() + "," + repository.getId()).name(repository.getId()).type(repository.getType()).layout(repository.getLayout())
                        .scope(repository.getScope()).build()).collect(Collectors.toList()));
                storageTreeForms.add(storageTreeForm);
            }
        }
        List<Repository> pageRepository = repositorieList.stream().skip((long) (page - 1) * limit).limit(limit).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(repositorieList)) {
            return new TableResultResponse<>(0, new ArrayList<>());
        }
        return new TableResultResponse<>(repositorieList.size(), pageRepository);
    }


    public boolean hasRepoAuth(String storageId, String repositoryId) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
        Storage storage = configurationManager.getStorage(storageId);
        if (Objects.isNull(storage)) {
            return false;
        }
        Repository repository = storage.getRepository(repositoryId);
        if (Objects.isNull(repository)) {
            return false;
        }
        UnicomRoleDTO userDetail = getUserDetail(userDetails.getEmail());
        if (userDetail == null) {
            return false;
        }
        return userDetail.ownProject().contains(repository.getProjectId());
    }

    public List<String> unicomResolveRepository() {
        List<String> repositoryIdList = Lists.newArrayList();
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
        if (UNICOM_SOURCE_ID.equals(userDetails.getSourceId())) {
            UnicomRoleDTO userDetail = getUserDetail(userDetails.getEmail());
            if (userDetail == null) {
                return repositoryIdList;
            }
            Set<String> projects = userDetail.ownProject();
            final List<Storage> storageList = new ArrayList<>(configurationManagementService.getConfiguration()
                    .getStorages()
                    .values());
            List<Repository> repositoryList = storageList.stream()
                    .filter(s -> (CollectionUtils.isNotEmpty(s.getRepositories().values()) && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()))))
                    .filter(storage -> MapUtils.isNotEmpty(storage.getRepositories()))
                    .flatMap(storage -> storage.getRepositories().entrySet().stream())
                    .map(Map.Entry::getValue)
                    .filter(repo -> projects.contains(repo.getProjectId()))
                    .collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(repositoryList)) {
                Storage storage;
                for (Repository repository : repositoryList) {
                    storage = repository.getStorage();
                    if (RepositoryScopeEnum.OPEN.getType().equals(repository.getScope())) {
                        repositoryIdList.add(String.format("%s-%s", storage.getId(), repository.getId()));
                    }
                }
            }
        }
        return repositoryIdList;
    }

    @Override
    public Collection<Privileges> getStorageAuthorities(String storageId, String repositoryId, List<String> paths) {
        if (hasRepoAuth(storageId, repositoryId)) {
            return Privileges.artifactsAll();
        } else {
            return Collections.emptyList();
        }
    }

    public Set<String> getArtifactoryPrivileges() {
        return Set.of("ARTIFACTS_DEPLOY", "ARTIFACTS_DELETE", "ARTIFACTS_VIEW", "ARTIFACTS_RESOLVE");
    }

    public boolean isUnicomUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        SpringSecurityUser userDetails = null;
        try {
            userDetails = (SpringSecurityUser) authentication.getPrincipal();
        } catch (Exception e) {
            return false;
        }
        return UNICOM_SOURCE_ID.equals(userDetails.getSourceId());

    }

    public ResponseEntity getStorageResponseEntity(StorageDto storage, Boolean filter, Authentication authentication) {
        if (filter) {
            SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
            UnicomRoleDTO unicomRoleDTO = getUserDetail(userDetails.getEmail());
            Map<String, ? extends Repository> repositoryMap = storage.getRepositories();
            Set<String> projects;
            if (unicomRoleDTO != null) {
                projects = unicomRoleDTO.ownProject();
            } else {
                projects = new HashSet<>();
            }
            if (Objects.nonNull(repositoryMap) && CollectionUtils.isNotEmpty(repositoryMap.values())) {
                repositoryMap = repositoryMap.values().stream()
                        .filter(item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()) || projects.contains(item.getProjectId()))
                        .map(item -> {
                            if (RepositoryTypeEnum.PROXY.getType().equals(item.getType())) {
                                RepositoryDto repositoryDto = (RepositoryDto) item;
                                repositoryDto.setHealthStatus(remoteRepositoryAlivenessCacheManager.isAlive(((RepositoryDto) item).getRemoteRepository()));
                            }
                            return item;
                        })
                        .collect(Collectors.toMap(Repository::getId, Function.identity()));
                storage.setRepositories((Map<String, RepositoryDto>) repositoryMap);
            }
        }
        Map<String, ? extends Repository> repositoryMap = storage.getRepositories();
        if (Objects.nonNull(repositoryMap) && CollectionUtils.isNotEmpty(repositoryMap.values())) {
            repositoryMap = repositoryMap.values().stream()
                    .map(item -> {
                        if (RepositoryTypeEnum.PROXY.getType().equals(item.getType())) {
                            RepositoryDto repositoryDto = (RepositoryDto) item;
                            repositoryDto.setHealthStatus(remoteRepositoryAlivenessCacheManager.isAlive(((RepositoryDto) item).getRemoteRepository()));
                        }
                        return item;
                    })
                    .collect(Collectors.toMap(Repository::getId, Function.identity()));
            storage.setRepositories((Map<String, RepositoryDto>) repositoryMap);
        }
        StorageData storageData = new StorageData(storage);
        return ResponseEntity.ok(storageData);
    }
}



