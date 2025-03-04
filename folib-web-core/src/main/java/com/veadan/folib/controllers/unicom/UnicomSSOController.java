package com.veadan.folib.controllers.unicom;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.domain.SecurityRoleEntity;
import com.veadan.folib.domain.User;
import com.veadan.folib.dto.AccessModelDTO;
import com.veadan.folib.dto.AccessResourcesDTO;
import com.veadan.folib.dto.AccessUsersDTO;
import com.veadan.folib.dto.RoleDTO;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.enums.StorageProviderEnum;
import com.veadan.folib.event.privilege.PrivilegeEventListenerRegistry;
import com.veadan.folib.forms.configuration.ProxyConfigurationForm;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.security.authentication.JwtTokenFetcher;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryPolicyEnum;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.security.JwtAuthenticationClaimsProvider;
import com.veadan.folib.users.security.JwtClaimsProvider;
import com.veadan.folib.users.security.SecurityTokenProvider;
import com.veadan.folib.users.service.FolibRoleService;
import com.veadan.folib.users.service.ResourceService;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.utils.UserUtils;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.util.Assert;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.view.RedirectView;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;
import javax.validation.groups.Default;
import java.io.IOException;
import java.nio.file.Files;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.Stack;
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

    public static final Map<String, Set<String>> layoutType = new HashMap<>();

    static {
        layoutType.put("raw", Set.of("**"));
        layoutType.put("maven 2", Set.of(".jar", ".war", ".pom"));
        layoutType.put("npm", Set.of(".tgz"));
        layoutType.put("rpm", Set.of(".rpm"));
        layoutType.put("docker", Set.of(".gz", ".tar", ".zip", ".giz"));
        layoutType.put("debian", Set.of(".deb"));
    }

    @Resource
    private FolibRoleService folibRoleService;

    @Resource
    private UnicomConfig unicomConfig;

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
    private ConversionService conversionService;

    static final String FAILED_SAVE_REPOSITORY = "The repository cannot be saved because the submitted form contains errors!";


    private static final String STORAGE_NOT_FOUND = "The storage was not found.";


    private static final String VIRTUAL_NAME = "-virtual";

    private static final String LOCAL_NAME = "-local";


    @Resource
    private UnicomAdapter unicomAdapter;

    @GetMapping("/login")
    public RedirectView ossLogin(@RequestParam String sessionId, HttpServletResponse response) {
        try {
            log.info("联通 login sessionId:{}", sessionId);
            UicomUserDTO uicomUserDTO = unicomAdapter.verify(sessionId);
            Assert.notNull(uicomUserDTO, "认证失败");
            UserDetails userDetails;
            try {
                userDetails = userDetailsService.loadUserByUsername(uicomUserDTO.getLoginName());
            } catch (UsernameNotFoundException e) {
                createIfNotExist(uicomUserDTO.getLoginName(), uicomUserDTO.getEmail());
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

    @GetMapping("/layout/type")
    public UnicomLayoutSupportVO getSupported(@RequestParam String layout) {
        UnicomLayoutSupportVO supportVO = new UnicomLayoutSupportVO();
        layout = layout.toLowerCase();
        Set<String> types = layoutType.get(layout);
        if (types == null || types.isEmpty()) {
            supportVO.setSupported(false);
            return supportVO;
        } else {
            supportVO.setSupported(true);
            supportVO.setTypes(types);
            return supportVO;
        }
    }

    private String getUIIndex() {
        String webUrlPrefix = System.getProperty(GlobalConstants.WEB_URL_PREFIX);
        if (StringUtils.isBlank(webUrlPrefix)) {
            webUrlPrefix = "/ui/";
        }
        return webUrlPrefix + "index.html";
    }

    // 创建仓库接口 增加创建人和应用Id
    @AuditLog(value = AuditEventNameEnum.ADD_REPOSITORY, target = "#storageId + '-'+ #repositoryId")
    @ApiOperation(value = "Adds or updates a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!"),
            @ApiResponse(code = 500, message = "Failed to remove the repository ${repositoryId}!")})
    @PostMapping(value = "/repo/{storageId}/{repositoryId}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<UnicomRepo>> addRepository(@ApiParam(value = "The storageId", required = true)
                                                          @PathVariable String storageId,
                                                          @ApiParam(value = "The repositoryId", required = true)
                                                          @PathVariable
                                                          String repositoryId,
                                                          @ApiParam(value = "The repository object", required = true)
                                                          @RequestBody
                                                          @Validated({Default.class,
                                                                  ProxyConfigurationForm.ProxyConfigurationFormChecks.class})
                                                          UnicomRepositroyForm repositoryForm,
                                                          BindingResult bindingResult,
                                                          @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            //生成子仓库和组合库
            Deque<RepositoryDto> completedRepo = new LinkedList<>();
            getUniqProjectEn(storage, repositoryForm);
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }
            repositoryForm.setStorageId(storageId);
            // 增加basedir
            if (StorageProviderEnum.S3.getType().equals(storage.getStorageProvider()) && StringUtils.isBlank(repositoryForm.getBasedir())) {
                String basedir = "/" + storageId + "/" + repositoryForm.getId();
                repositoryForm.setBasedir(basedir);
            }
            RepositoryDto repository = conversionService.convert(repositoryForm, RepositoryDto.class);
            if (Objects.isNull(repository)) {
                return getFailedResponseEntity(HttpStatus.BAD_REQUEST, "The repository params is null", accept);
            }
            try {
                if (Objects.nonNull(repositoryForm.getSubRepoList()) && !repositoryForm.getSubRepoList().isEmpty()) {
                    log.info("创建子仓库");
                    for (UnicomRepositroyForm.SubRepo subRepo : repositoryForm.getSubRepoList()) {
                        RepositoryDto subRepository = conversionService.convert(repositoryForm, RepositoryDto.class);
                        subRepository.setId(subRepo.getId());
                        subRepository.setPolicy(subRepo.getPolicy());
                        subRepository.setType(RepositoryTypeEnum.HOSTED.getType());
                        RepositoryDto syncRepo = createRepository(storage, subRepository);
                        completedRepo.addLast(syncRepo);
                        String storageIdAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(storageId, subRepo.getId());
                        repository.getGroupRepositories().add(storageIdAndRepositoryId);
                    }
                }
                // 如果local库只有一个设置为默认库
                if (repository.getGroupRepositories().size() == 1) {
                    for (String groupRepository : repository.getGroupRepositories()) {
                        repository.setGroupDefaultRepository(groupRepository);
                    }
                }
                // 将代理库添加到组合库里 获取当前代理layout的所有代理库
                List<Repository> proxyRepos = storage.getRepositories().values().stream()
                        .filter(repo -> repositoryForm.getLayout().equals(repo.getLayout()))
                        .filter(repo -> RepositoryTypeEnum.PROXY.getType().equals(repo.getType()))
                        .collect(Collectors.toList());
                for (Repository proxyRepo : proxyRepos) {
                    String storageIdAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(storageId, proxyRepo.getId());
                    repository.getGroupRepositories().add(storageIdAndRepositoryId);
                }
                RepositoryDto syncRepositoryDto = createRepository(storage, repository);
                completedRepo.addLast(syncRepositoryDto);
            } catch (Exception e) {
                log.info("创建仓库异常{}", e.getMessage(), e);
                //删除已经创建的仓库
                while (!completedRepo.isEmpty()) {
                    RepositoryDto repo = completedRepo.pop();
                    try {
                        configurationManagementService.removeRepository(repo.getStorage().getId(), repo.getId());
                    } catch (IOException ex) {
                        log.error("仓库{}删除异常,请手动删除", repo.getId());
                    }
                }
                String message = String.format("自动创建项目【%s】制品仓库失败,请联系相关人员手动创建", repositoryForm.getProjectName());
                unicomAdapter.sendMessageEmail("创建仓库异常", message, repositoryForm.getEmail());
            }
            // 同步信息到其他节点
            while (!completedRepo.isEmpty()) {
                RepositoryDto first = completedRepo.pollFirst();
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(first, first.getStorage().getId(), first.getId(), SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
                //同步资源信息到其他节点
                privilegeEventListenerRegistry.dispatchResourceSyncEvent(first.getStorage().getId() + "_" + first.getId());
            }
            // 创建一个新的用户并赋予权限
            UserDto projectUser = createRole(repositoryForm);
            sendEmail(projectUser, repositoryForm,repository.getSubLayout());
            return ResponseEntity.ok(repositoryForm.genRepoInfo());
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    public RepositoryDto createRepository(Storage storage, RepositoryDto repository) throws IOException {
        String storageId = storage.getId();
        if (Objects.isNull(repository)) {
            return null;
        }
        String repositoryId = repository.getId();
        repository.setSyncEnabled(true);
        if (repository.getArtifactMaxSize() == 0) {
            repository.setArtifactMaxSize(107374182400L);
        }
        Repository subExist = storage.getRepository(repository.getId());
        if (Objects.nonNull(subExist)) {
            return null;
        }
        RepositoryDto subRepositoryDto;
        groupRepositoryValid(storageId, repository);
        configurationManagementService.saveRepository(storageId, repository);
        subRepositoryDto = getMutableConfigurationClone().getStorage(storageId)
                .getRepository(repository.getId());
        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(new RepositoryData(repository));
        try {
            if (!Files.exists(repositoryPath)) {
                repositoryManagementService.createRepository(storageId, repositoryId);
            }
        } catch (Exception ex) {
            logger.error("Failed to create the repository path {}!", repository.getId(), ex);
            try {
                configurationManagementService.removeRepository(storageId, repositoryId);
            } catch (Exception e) {
                logger.error("Failed to remove the repository {}!", repositoryId, e);
            }
            throw new RuntimeException(ex.getMessage());
        }
        if (!RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
            //初始化仓库数据
            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repository.getLayout());
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
        return subRepositoryDto;
    }

    private void groupRepositoryValid(String storageId, Repository repository) {
        if (Objects.isNull(repository) || CollectionUtils.isEmpty(repository.getGroupRepositories())) {
            return;
        }
        String storageIdAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(storageId, repository.getId());
        if (repository.getGroupRepositories().contains(storageIdAndRepositoryId)) {
            throw new IllegalArgumentException("The combination repository cannot contain itself");
        }
    }

    private void sendEmail(UserDto userDto, UnicomRepositroyForm form,String subLayout) {
        String title = String.format("项目【%s】,folib仓库信息", form.getProjectName());
        UnicomEmailDTO emailDTO = new UnicomEmailDTO();
        emailDTO.setTitle(title);
        emailDTO.setAccount(form.getEmail());
        // 获取仓库地址
        String baseUrl = StringUtils.chomp(configurationManagementService.getConfiguration().getBaseUrl(), "/");
        String prefixUrl = baseUrl + getLayoutRepoPrefix(subLayout);
        StringBuilder localLink = new StringBuilder();
        String releaseRepo = "";
        String snapshotRepo = "";
        for (UnicomRepositroyForm.SubRepo subRepo : form.getSubRepoList()) {
            localLink.append(prefixUrl).append(subRepo.getId()).append("\n");
            if (RepositoryPolicyEnum.RELEASE.getPolicy().equals(subRepo.getPolicy())) {
                releaseRepo = prefixUrl + subRepo.getId();
            }
            if (RepositoryPolicyEnum.SNAPSHOT.getPolicy().equals(subRepo.getPolicy())) {
                snapshotRepo = prefixUrl + subRepo.getId();
            }

        }
        String template = EmailTemplate.getTemplateByLayout(form.getLayout(), form.getSubLayout());
        String content = template.replace(EmailTemplate.USERNAME, userDto.getUsername())
                .replace(EmailTemplate.PASSWORD, userDto.getPassword())
                .replace(EmailTemplate.BASE64_PASSWORD, userDto.getOriginalPassword())
                .replace(EmailTemplate.GROUP_LINK, prefixUrl + form.getId())
                .replace(EmailTemplate.LOCAL_LINK, localLink.toString())
                .replace(EmailTemplate.REPOSITORY_ID, form.getId())
                .replace(EmailTemplate.RELEASE_REPO, releaseRepo)
                .replace(EmailTemplate.SNAPSHOT_REPO, snapshotRepo);
        emailDTO.setContent(content);
        log.info("发送的文本正文是:\n{}", content);
        // 发送邮件
        unicomAdapter.sendEmail(emailDTO);
    }


    public String generateRawPassword() {
        final String UPPERCASE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        final String LOWERCASE = "abcdefghijklmnopqrstuvwxyz";
        final String DIGITS = "0123456789";
        final String SPECIAL_CHARACTERS = "!@#$%^&?";
        final String ALL_CHARACTERS = UPPERCASE + LOWERCASE + DIGITS + SPECIAL_CHARACTERS;
        SecureRandom random = new SecureRandom();
        StringBuilder password = new StringBuilder(15);
        password.append(UPPERCASE.charAt(random.nextInt(UPPERCASE.length())));
        password.append(LOWERCASE.charAt(random.nextInt(LOWERCASE.length())));
        password.append(DIGITS.charAt(random.nextInt(DIGITS.length())));
        password.append(SPECIAL_CHARACTERS.charAt(random.nextInt(SPECIAL_CHARACTERS.length())));
        for (int i = 4; i < 15; i++) {
            password.append(ALL_CHARACTERS.charAt(random.nextInt(ALL_CHARACTERS.length())));
        }
        return shuffleString(password.toString(), random);
    }

    private String shuffleString(String input, SecureRandom random) {
        char[] characters = input.toCharArray();
        for (int i = characters.length - 1; i > 0; i--) {
            int j = random.nextInt(i + 1);
            char temp = characters[i];
            characters[i] = characters[j];
            characters[j] = temp;
        }
        return new String(characters);
    }


    @GetMapping(value = "/token")
    public ResponseEntity<UnicomToken> getTokenBySessionId(@RequestParam String sessionId) {
        try {
            log.info("联通 获取token sessionId:{}", sessionId);
            UicomUserDTO uicomUserDTO = unicomAdapter.verify(sessionId);
            Assert.notNull(uicomUserDTO, "认证失败");
            UserDetails userDetails;
            try {
                userDetails = userDetailsService.loadUserByUsername(uicomUserDTO.getLoginName());
            } catch (UsernameNotFoundException e) {
                createIfNotExist(uicomUserDTO.getLoginName(), uicomUserDTO.getEmail());
                userDetails = userDetailsService.loadUserByUsername(uicomUserDTO.getLoginName());
            }
            Integer timeout = configurationManager.getSessionTimeoutSeconds();
            Map<String, String> claims = jwtClaimsProvider.getClaims((SpringSecurityUser) userDetails);
            String token = securityTokenProvider.getToken(userDetails.getUsername(), claims, timeout, null);
            //存储JWT令牌到Cookie
            UnicomToken unicomToken = new UnicomToken();
            unicomToken.setToken(token);
            return ResponseEntity.ok(unicomToken);
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage());
        }
    }


    @GetMapping(value = "/getRedirectUrl")
    public ResponseEntity<String> getRedirectUrl() {
        log.info("单点登录地址是{}",unicomConfig.getRedirectUrl());
        return ResponseEntity.ok(unicomConfig.getRedirectUrl());

    }

    private void createIfNotExist(String loginName, String email) {
        User exist = userService.findByUsername(loginName);
        if (Objects.isNull(exist)) {
            log.info("用户【{}】不存在,开始创建用户", loginName);
            UserDto user = new UserDto();
            user.setEmail(email);
            user.setUsername(loginName);
            user.setEnabled(true);
            String pwd = generateRawPassword();
            user.setPassword(pwd);
            user.setOriginalPassword(Base64.encodeBase64String(pwd.getBytes()));
            user.setSourceId(UnicomAdapter.UNICOM_SOURCE_ID);
            SecurityRole securityRole = new SecurityRoleEntity(SystemRole.GENERAL.name());
            user.setRoles(Collections.singleton(securityRole));
            userService.save(new EncodedPasswordUser(user, passwordEncoder));
            //同步用户信息到其他节点
            privilegeEventListenerRegistry.dispatchUserSyncEvent(user.getUuid());
        }
    }

    private UserDto createRole(UnicomRepositroyForm form) {
        RoleDTO roleDTO = new RoleDTO();
        roleDTO.setName(form.getProjectId() + "_" + form.getSubLayout());
        String description = String.format("项目【%s】-【%s】", form.getProjectName(), form.getSubLayout());
        roleDTO.setDescription(description);
        // 资源
        List<AccessResourcesDTO> resources = new LinkedList<>();
        for (UnicomRepositroyForm.SubRepo subRepo : form.getSubRepoList()) {
            AccessResourcesDTO resourcesDTO = new AccessResourcesDTO();
            resourcesDTO.setStorageId(form.getStorageId());
            resourcesDTO.setRepositoryId(subRepo.getId());
            resources.add(resourcesDTO);
        }
        roleDTO.setResources(resources);
        //用户
        // 创建项目级别的用户
        String repoId = form.getId();
        String[] split = repoId.split("-");
        String name = split[0];
        // 判断用户是否存在存在则在用户名后加一个0-9数字，直到用户不存在
        while (Objects.nonNull(userService.findByUsername(name))) {
            name = name + (int) (Math.random() * 10);
        }
        String pwd = generateRawPassword();
        UserDto user = new UserDto();
        user.setUsername(name);
        user.setEnabled(true);
        user.setPassword(pwd);
        user.setOriginalPassword(Base64.encodeBase64String(pwd.getBytes()));
        SecurityRole securityRole = new SecurityRoleEntity(SystemRole.GENERAL.name());
        user.setRoles(Collections.singleton(securityRole));
        userService.save(new EncodedPasswordUser(user, passwordEncoder));
        //同步用户信息到其他节点
        privilegeEventListenerRegistry.dispatchUserSyncEvent(user.getUuid());
        AccessModelDTO accessModelDTO = new AccessModelDTO();
        List<AccessUsersDTO> users = new LinkedList<>();
        AccessUsersDTO usersDTO = new AccessUsersDTO();
        usersDTO.setId(user.getUsername());
        usersDTO.setAccess(Arrays.asList("ARTIFACTS_DEPLOY", "ARTIFACTS_RESOLVE", "ARTIFACTS_VIEW", "ARTIFACTS_DELETE"));
        users.add(usersDTO);
        accessModelDTO.setUsers(users);
        roleDTO.setPrivileges(accessModelDTO);
        folibRoleService.save(roleDTO, UserUtils.getUsername());
        //同步角色信息到其他节点
        privilegeEventListenerRegistry.dispatchRoleSyncEvent(roleDTO.getName());
        return user;

    }


    private void getUniqProjectEn(Storage storage, UnicomRepositroyForm form) {
        String projectEn = form.getId();
        String suffix = "-" + form.getSubLayout() + VIRTUAL_NAME;
        while (Objects.nonNull(storage.getRepository(projectEn + suffix))) {
            projectEn = projectEn + (int) (Math.random() * 10);
        }
        form.setId(projectEn + suffix);
        List<UnicomRepositroyForm.SubRepo> repos = new LinkedList<>();
        if (MavenArtifactCoordinates.LAYOUT_NAME.equals(form.getLayout())) {
            UnicomRepositroyForm.SubRepo release = new UnicomRepositroyForm.SubRepo();
            release.setId(projectEn + "-" + form.getSubLayout() + "-releases" + LOCAL_NAME);
            release.setPolicy(RepositoryPolicyEnum.RELEASE.getPolicy());
            repos.add(release);
            UnicomRepositroyForm.SubRepo snapshot = new UnicomRepositroyForm.SubRepo();
            snapshot.setId(projectEn + "-" + form.getSubLayout() + "-snapshots" + LOCAL_NAME);
            snapshot.setPolicy(RepositoryPolicyEnum.SNAPSHOT.getPolicy());
            repos.add(snapshot);
        } else {
            UnicomRepositroyForm.SubRepo generic = new UnicomRepositroyForm.SubRepo();
            generic.setId(projectEn + "-" + form.getSubLayout() + LOCAL_NAME);
            generic.setPolicy(RepositoryPolicyEnum.MIXED.getPolicy());
            repos.add(generic);
        }
        form.setSubRepoList(repos);
    }

    private String getLayoutRepoPrefix(String subLayout) {
        String prefix;
        switch (subLayout) {
            case "pypi":
                prefix = "/artifactory/api/pypi/";
                break;
            case "npm":
                prefix = "/artifactory/api/npm/";
                break;
            case "ohpm":
                prefix = "/artifactory/api/ohpm/";
                break;
            case "php":
                prefix = "/artifactory/api/composer/";
                break;
            case "conan":
                prefix = "/artifactory/api/conan/";
                break;
            case "helm":
                prefix = "/artifactory/api/helm/";
                break;
            case "cocoapods":
                prefix = "/artifactory/api/pods/";
                break;
            case "go":
                prefix = "/artifactory/api/go/";
                break;
            case "gitlfs":
                prefix = "/artifactory/api/lfs/";
                break;
            case "huggingface":
                prefix = "/artifactory/api/huggingfaceml/";
                break;
            case "pub":
                prefix = "/artifactory/api/pub/";
                break;
            case "docker":
                prefix = "";
                break;
            case "cargo":
                prefix = "/artifactory/api/cargo/";
                break;
            default:
                prefix = "/artifactory/";
        }
        return prefix;
    }
}
