package com.veadan.folib.controllers.unicom;

import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.cluster.SyncStorageEnum;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.controllers.cluster.dto.SyncStorageDto;
import com.veadan.folib.entity.FolibRole;
import com.veadan.folib.entity.RoleResourceRef;
import com.veadan.folib.enums.StorageProviderEnum;
import com.veadan.folib.event.privilege.PrivilegeEventListenerRegistry;
import com.veadan.folib.forms.configuration.StorageForm;
import com.veadan.folib.providers.io.RepositoryFileSystemRegistry;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.security.authentication.JwtTokenFetcher;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.security.JwtAuthenticationClaimsProvider;
import com.veadan.folib.users.security.JwtClaimsProvider;
import com.veadan.folib.users.security.SecurityTokenProvider;
import com.veadan.folib.users.service.FolibRoleService;
import com.veadan.folib.users.service.ResourceService;
import com.veadan.folib.users.service.RoleResourceRefService;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.util.Assert;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.servlet.view.RedirectView;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 联通单点登录需求
 *
 * @author huayanjun
 * @since 2024-09-20 11:07
 */


@Slf4j
@RestController
@RequestMapping("/api/unicom")
@Api(tags = "通联单点登录")
public class UnicomSSOController extends BaseController {

    @Resource
    private RestTemplate restTemplate;

    @Resource
    private UnicomConfig unicomConfig;

    @Resource
    private StorageManagementService storageManagementService;


    @Resource
    private RoleResourceRefService roleResourceRefService;

    @Resource
    private FolibRoleService folibRoleService;

    @Resource
    private SecurityTokenProvider securityTokenProvider;

    @Resource
    @JwtAuthenticationClaimsProvider.JwtAuthentication
    private JwtClaimsProvider jwtClaimsProvider;

    @Resource
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Resource
    private UserDetailsService userDetailsService;

    @Resource
    private ClusterSyncService clusterSyncService;

    @Resource
    private PasswordEncoder passwordEncoder;

    @Resource
    private ConfigurationManagementService configurationManagementService;

    @Resource
    private PrivilegeEventListenerRegistry privilegeEventListenerRegistry;

    @Resource
    private RepositoryManagementService repositoryManagementService;

    @Resource
    private LayoutProviderRegistry layoutProviderRegistry;

    @Resource
    private ResourceService resourceService;

    @Resource
    private RepositoryFileSystemRegistry repositoryFileSystemRegistry;


    private final String PREFIX = "unicom-";


    @GetMapping("/login")
    public RedirectView ossLogin(@RequestParam String sessionId, HttpServletResponse response) {
        try {
            UicomUserDTO uicomUserDTO = verify(sessionId);
            Assert.notNull(uicomUserDTO, "认证失败");
            UserDetails userDetails = null;
            try {
                userDetails = userDetailsService.loadUserByUsername(uicomUserDTO.getLoginName());
            } catch (UsernameNotFoundException e) {
                UserDto user = new UserDto();
                user.setEmail(uicomUserDTO.getEmail());
                user.setUsername(uicomUserDTO.getLoginName());
                user.setEnabled(true);
                user.setPassword(uicomUserDTO.getMobile());
                user.setUserGroupIds(Collections.singleton("1"));
                userService.save(new EncodedPasswordUser(user, passwordEncoder));
                //同步用户信息到其他节点
                privilegeEventListenerRegistry.dispatchUserSyncEvent(user.getUuid());
                //todo 更新权限
                //重新获取userdetail
                userDetails = userDetailsService.loadUserByUsername(uicomUserDTO.getLoginName());
            }
            Integer timeout = configurationManager.getSessionTimeoutSeconds();
            Map<String, String> claims = jwtClaimsProvider.getClaims((SpringSecurityUser) userDetails);
            String token = securityTokenProvider.getToken(uicomUserDTO.getLoginName(), claims, timeout, null);
            //存储JWT令牌到Cookie
            Cookie cookie = new Cookie(JwtTokenFetcher.AUTHORIZATION_COOKIE, token);
            cookie.setPath("/");
            cookie.setMaxAge(timeout);
            response.addCookie(cookie);
        } catch (Exception e) {
            log.error("sso login failed,sessionId:{},error:{}", sessionId, e.getMessage(), e);
            response.setStatus(HttpStatus.UNAUTHORIZED.value());
        }
        //跳转到首页

        return new RedirectView(getUIIndex(), true, false);
    }


    // 根据应用创建存储空间和仓库
    @PostMapping("/storage")
    public ResponseEntity<UnicomStorageVO> createStorage(@RequestBody @Validated({StorageForm.NewStorage.class}) UnicomStorageDTO storageDTO) {
        try {
            // 判断布局是否存在
            for (String layout : storageDTO.getLayouts()) {
                RepositoryDto dto = new RepositoryDto();
                dto.setLayout(layout);
                Assert.notNull(repositoryFileSystemRegistry.lookupRepositoryFileSystemProviderFactory(dto), "不支持的布局" + layout);
            }
            // 获取存储空间信息
            StorageDto storage = new StorageDto();
            String storageId = storageDTO.getId();
            storage.setId(storageId);
            if (org.apache.commons.lang3.StringUtils.isBlank(storage.getStorageProvider())) {
                storage.setStorageProvider(StorageProviderEnum.LOCAL.getType());
            }
            // todo 根据项目获取 管理人员和项目成员 调用研发平台
            List<String> users = getUserByProject(storageId);
            storage.setAdmin("admin");
            storage.setUsers(new HashSet<>(users));
            // 创建存储空间
            storageManagementService.createStorage(storage);
            // 向其他集群节点同步storage
            SyncStorageDto syncStorageDto = new SyncStorageDto(storage, storageId, SyncStorageEnum.CREATE);
            clusterSyncService.syncStorage(syncStorageDto);
            //同步资源信息到其他节点
            privilegeEventListenerRegistry.dispatchResourceSyncEvent(storage.getId());
            // 遍历布局建立仓库
            HashMap<String, String> urls = new HashMap<>();
            // 创建仓库
            String baseUrl = configurationManager.getConfiguration().getBaseUrl();
            if (!baseUrl.endsWith("/")) {
                baseUrl = baseUrl + "/" + storageId + "/";
            } else {
                baseUrl = baseUrl + storageId + "/";
            }
            List<UnicomStorageVO.ProjectLayout> result = new LinkedList<>();
            UnicomStorageVO vo = new UnicomStorageVO();
            if (storageDTO.getLayouts() != null) {
                for (String layout : storageDTO.getLayouts()) {
                    RepositoryDto repositoryDto = new RepositoryDto();
                    String alias=layout.contains(" ")?layout.substring(0, layout.indexOf(" ")):layout;
                    alias =alias.toLowerCase();
                    String repositoryId = PREFIX + storageId + "-" + alias;
                    repositoryDto.setId(repositoryId);
                    repositoryDto.setArtifactMaxSize(107374182400L);
                    repositoryDto.setLayout(layout);
                    configurationManagementService.saveRepository(storageId, repositoryDto);
                    RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                            .getRepository(repositoryId);
                    final RepositoryPath repositoryPath = repositoryPathResolver.resolve(new RepositoryData(repository));
                    try {
                        if (!Files.exists(repositoryPath)) {
                            repositoryManagementService.createRepository(storageId, repositoryId);
                        }
                    } catch (Exception ex) {
                        logger.error("Failed to create the repository path {}!", repositoryId, ex);
                        try {
                            configurationManagementService.removeRepository(storageId, repositoryId);
                        } catch (Exception e) {
                            logger.error("Failed to remove the repository {}!", repositoryId, e);
                        }
                        throw new RuntimeException(ex.getMessage());
                    }
                    if (!RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                        //初始化仓库数据
                        LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repositoryDto.getLayout());
                        layoutProvider.initData(storageId, repositoryId);
                    }
                    String resourceId = storageId + "_" + repositoryId;
                    com.veadan.folib.entity.Resource resource = resourceService.queryById(resourceId);
                    if (Objects.equals(null, resource)) {
                        resourceService.insert(com.veadan.folib.entity.Resource.builder()
                                .id(resourceId.toUpperCase())
                                .storageId(storageId)
                                .repositoryId(repositoryId)
                                .build());
                    }
                    SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repositoryDto, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
                    clusterSyncService.syncRepository(syncRepositoryDto);
                    //同步资源信息到其他节点
                    privilegeEventListenerRegistry.dispatchResourceSyncEvent(storageId + "_" + repositoryId);
                    String url = baseUrl + repositoryId;
                    UnicomStorageVO.ProjectLayout p = new UnicomStorageVO.ProjectLayout();
                    p.setName(layout);
                    p.setAddress(url);
                    result.add(p);
                }
                vo.setLayouts(result);
            }
            return ResponseEntity.ok(vo);
        } catch (Exception e) {
            log.error("create repository failed {}", e.getMessage(), e);
            throw new RuntimeException(e.getMessage());
        }

    }


    @PostMapping("/storage/permission")
    public ResponseEntity<String> updatePermission(@RequestBody UnicomPermissionDTO unicomPermissionDTO) {
        try {
            String sourceId = unicomPermissionDTO.getProjectId().toUpperCase();
            String key = String.format("STORAGE_USER_%S", sourceId);
            FolibRole folibRole = folibRoleService.queryById(key);
            Assert.notNull(folibRole, "找不到对应项目");
            roleResourceRefService.deleteAllByRoleIdAndEntityNotNull(key);
            try {
                EnumSet<Privileges> storagePrivileges = Privileges.storageUser();
                Set<String> privileges = storagePrivileges.stream().map(Privileges::getAuthority).collect(Collectors.toSet());
                for (String username : unicomPermissionDTO.getUsers()) {
                    List<RoleResourceRef> roleResourceRefs = privileges.stream().map(privilege -> RoleResourceRef.builder().roleId(key).entityId(username).refType(GlobalConstants.ROLE_TYPE_USER).resourceId(sourceId)
                            .storagePrivilege(privilege).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build()).collect(Collectors.toList());
                    roleResourceRefs.add(RoleResourceRef.builder().roleId(key).resourceId(sourceId).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build());
                    roleResourceRefService.saveBath(roleResourceRefs);
                }
            } catch (Exception ex) {
                logger.error("handler storage {} user role error：{}", sourceId, ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex.getMessage());
            }

        } catch (Exception e) {
            log.error("projectId:{} sync permission failed{}", unicomPermissionDTO.getProjectId(), e.getMessage(), e);
            throw new RuntimeException("sync permission failed");
        }
        return ResponseEntity.ok("sync success");
    }


    public UicomUserDTO verify(String sessionId) {
//        try {
//            HttpHeaders header = getHeader();
//            // 这里需要将sessionId放到query参数中，
//            String url = unicomConfig.getVerifyUrl() + "?sessionId=" + sessionId;
//            HttpEntity<String> entity = new HttpEntity<>(header);
//            ResponseEntity<UicomUserDTO> response = restTemplate.exchange(url, HttpMethod.POST, entity, UicomUserDTO.class);
//            if (response.getStatusCode() == HttpStatus.OK) {
//                log.debug("verify success,sessionId:{}", sessionId);
//                return response.getBody();
//            } else {
//                return null;
//            }
//        } catch (Exception e) {
//            log.error("verify failed,sessionId:{},error:{}", sessionId, e.getMessage(), e);
//            return null;
//        }
        UicomUserDTO dto = new UicomUserDTO();
        dto.setName("huahua");
        dto.setEmail("huayan@163.com");
        dto.setLoginName("hua");
        dto.setMobile("152529172");
        return dto;
    }

    /**
     * @param loginName 登录名
     */
    public UicomUserDetail getUserDetail(String loginName) {
        try {
            HttpHeaders header = getHeader();
            String url = unicomConfig.getUserDetailUrl() + "?loginName=" + loginName;
            HttpEntity<String> entity = new HttpEntity<>(header);
            ResponseEntity<UicomUserDetail> response = restTemplate.exchange(url, HttpMethod.POST, entity, UicomUserDetail.class);
            if (response.getStatusCode() == HttpStatus.OK) {
                log.debug("get user: {} detail success", loginName);
                return response.getBody();
            } else {
                return null;
            }
        } catch (Exception e) {
            log.debug("get user: {} detail error:{}", loginName, e.getMessage(), e);
            return null;
        }
    }

    public List<String> getUserByProject(String projectId) {
//        try {
//            HttpHeaders header = getHeader();
//            String url = unicomConfig.getUserByProjectUrl() + "?projectId=" + projectId;
//            HttpEntity<String> entity = new HttpEntity<>(header);
//            ResponseEntity<List> response = restTemplate.exchange(url, HttpMethod.POST, entity, List.class);
//            if (response.getStatusCode() == HttpStatus.OK) {
//                log.debug("get user by project {} success", projectId);
//                return response.getBody();
//            } else {
//                return null;
//            }
//        } catch (Exception e) {
//            log.error("get user by project {} failed:{}", projectId,e.getMessage(),e);
//            return null;
//        }
        return Collections.singletonList("test1");

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
            log.debug("Sha256Gen,apCode:{},appSecret:{},timeStamp:{},signature:{}", appCode, appSecret, timeStamp, signature);
            header.add("appId", unicomConfig.getAppCode());
            header.add("signature", signature);
            header.add("timestamp", timeStamp);
        } catch (Exception e) {
            log.error("Sha256Gen failed:{}", e.getMessage(), e);
        }
        return header;
    }

    private String getUIIndex() {
        String webUrlPrefix = System.getProperty(GlobalConstants.WEB_URL_PREFIX);
        if (StringUtils.isBlank(webUrlPrefix)) {
            webUrlPrefix = "/ui/";
        }
        return webUrlPrefix + "index.html";
    }


}
