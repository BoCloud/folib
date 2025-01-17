package com.veadan.folib.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.components.auth.AuthComponent;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.configuration.MutableConfiguration;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.support.ErrorResponseEntityBody;
import com.veadan.folib.controllers.support.ListEntityBody;
import com.veadan.folib.controllers.support.ResponseEntityBody;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.domain.FileContent;
import com.veadan.folib.enums.RepositoryScopeEnum;
import com.veadan.folib.exception.ExceptionHandlingOutputStream;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathLock;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.DirectoryListingService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import javax.inject.Inject;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Provides common subroutines that will be useful for any backend controllers.
 *
 * @author
 * @author Veadan
 * @author veadan
 */
public abstract class BaseController {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Inject
    protected ObjectMapper objectMapper;

    @Inject
    protected ConfigurationManagementService configurationManagementService;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected RepositoryPathLock repositoryPathLock;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private DirectoryListingService directoryListingService;

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    @Lazy
    private AuthComponent authComponent;

    protected Configuration getConfiguration() {
        return configurationManagementService.getConfiguration();
    }

    protected Storage getStorage(String storageId) {
        return configurationManager.getConfiguration().getStorage(storageId);
    }

    protected Repository getRepository(String storageId,
                                       String repositoryId) {
        return getStorage(storageId).getRepository(repositoryId);
    }

    protected MutableConfiguration getMutableConfigurationClone() {
        return configurationManagementService.getMutableConfigurationClone();
    }

    /**
     * You will rarely need to use this method directly. In most cases you should consider using the methods below
     * to guarantee consistency in the returned responses to the client request.
     *
     * @param message      Message to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return Object
     */
    protected Object getResponseEntityBody(String message,
                                           String acceptHeader) {
        if (acceptHeader != null && !acceptHeader.isEmpty()) {
            acceptHeader = acceptHeader.toLowerCase();
            if ((acceptHeader.contains(MediaType.TEXT_PLAIN_VALUE.toLowerCase()) ||
                    acceptHeader.contains(MediaType.TEXT_HTML_VALUE.toLowerCase()))) {
                return message;
            }
        }

        return new ResponseEntityBody(message);
    }

    /**
     * @param fieldName JSON field name
     * @param list      the list
     * @return
     */
    protected ResponseEntity getJSONListResponseEntityBody(String fieldName,
                                                           List<?> list) {
        return ResponseEntity.ok(new ListEntityBody(fieldName, list));
    }

    /**
     * @param fieldName JSON field name
     * @param iterable  the iterable
     * @return ResponseEntity
     */
    protected ResponseEntity getJSONListResponseEntityBody(String fieldName,
                                                           final Iterable<?> iterable) {
        List<?> list = IteratorUtils.toList(iterable.iterator());
        return getJSONListResponseEntityBody(fieldName, list);
    }

    /**
     * Used for operations which have been successfully performed.
     *
     * @param message      Success to be returned to the client.
     * @param headers      response headers
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getSuccessfulResponseEntity(String message, HttpHeaders headers, String acceptHeader) {
        return ResponseEntity.ok().headers(headers).body(getResponseEntityBody(message, acceptHeader));
    }

    /**
     * Used for operations which have been successfully performed.
     *
     * @param message      Success to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getSuccessfulResponseEntity(String message,
                                                         String acceptHeader) {
        return getSuccessfulResponseEntity(message, null, acceptHeader);
    }

    /**
     * Used for operations which have failed.
     *
     * @param status       Status code to be returned (i.e. 400 Bad Request)
     * @param message      Error to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getFailedResponseEntity(HttpStatus status,
                                                     String message,
                                                     String acceptHeader) {
        return ResponseEntity.status(status)
                .body(getResponseEntityBody(message, acceptHeader));
    }

    /**
     * @param message      Error to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getBadRequestResponseEntity(String message,
                                                         String acceptHeader) {
        return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, acceptHeader);
    }

    /**
     * Used in cases where resource could not be found.
     *
     * @param message      Error to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getNotFoundResponseEntity(String message,
                                                       String acceptHeader) {
        return ResponseEntity.status(HttpStatus.NOT_FOUND)
                .body(getResponseEntityBody(message, acceptHeader));

    }

    /**
     * Used in cases where resource is not available not be found.
     *
     * @param message      Error to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getServiceUnavailableResponseEntity(String message,
                                                                 String acceptHeader) {
        return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                .body(getResponseEntityBody(message, acceptHeader));
    }

    /**
     * @param message      Error message to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getRuntimeExceptionResponseEntity(String message,
                                                               String acceptHeader) {
        return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR,
                new RuntimeException(message),
                acceptHeader);
    }

    /**
     * @param httpStatus   HttpStatus to be returned.
     * @param cause        Exception.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getExceptionResponseEntity(HttpStatus httpStatus,
                                                        Throwable cause,
                                                        String acceptHeader) {
        return getExceptionResponseEntity(httpStatus, cause.getMessage(), cause, acceptHeader);
    }

    /**
     * @param httpStatus   HttpStatus to be returned.
     * @param message      Error message to display in the logs and returned to the client.
     * @param cause        Exception.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getExceptionResponseEntity(HttpStatus httpStatus,
                                                        String message,
                                                        Throwable cause,
                                                        String acceptHeader) {
        logger.error(message, cause);

        Object responseEntityBody = getResponseEntityBody(message, acceptHeader);
        return ResponseEntity.status(httpStatus)
                .body(responseEntityBody);
    }

    /**
     * Used to stream files to the client.
     *
     * @param is       InputStream
     * @param filename String
     * @return ResponseEntity
     * @throws IllegalStateException
     */
    protected ResponseEntity<InputStreamResource> getStreamToResponseEntity(InputStream is,
                                                                            String filename)
            throws IllegalStateException {
        InputStreamResource inputStreamResource = new InputStreamResource(is);

        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.set("Content-Disposition", "attachment; filename=" + filename);

        return new ResponseEntity<>(inputStreamResource, httpHeaders, HttpStatus.OK);
    }

    // TODO: The methods below are obsolete and should be gradually removed from usage. We'll maybe only keep copyToResponse.
    protected ResponseEntity toResponseEntityError(String message,
                                                   HttpStatus httpStatus) {
        return ResponseEntity.status(httpStatus)
                .body(new ErrorResponseEntityBody(message));
    }

    protected ResponseEntity toResponseEntityError(String message) {
        return toResponseEntityError(message, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    public static void copyToResponse(InputStream is,
                                      HttpServletResponse response)
            throws IOException {
        try (
                OutputStream os = new ExceptionHandlingOutputStream(response.getOutputStream());
                WritableByteChannel outputChannel = Channels.newChannel(os)
        ) {
            ReadableByteChannel inputChannel = Channels.newChannel(is);
            ByteBuffer buffer = ByteBuffer.allocate(8192);
            while (inputChannel.read(buffer) != -1) {
                buffer.flip();
                outputChannel.write(buffer);
                buffer.clear();
            }
            response.flushBuffer();
        }
    }

    public Set<String> roleNames(Authentication authentication) {
        SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
        return Optional.ofNullable(userDetails.getRoles()).orElse(Collections.emptySet()).stream().map(Role::getName).collect(Collectors.toSet());
    }

    public boolean hasAdmin() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return false;
        }
        Object o = authentication.getPrincipal();
        if (!(o instanceof SpringSecurityUser)) {
            return false;
        }
        SpringSecurityUser userDetails = (SpringSecurityUser) o;
        if (CollectionUtils.isEmpty(userDetails.getRoles())) {
            return false;
        }
        return userDetails.getRoles().stream().anyMatch(item -> SystemRole.ADMIN.name().equals(item.getName()));
    }

    public boolean hasRepositoryResolve(Repository repository) {
        return validatePathPrivileges(repository, null, Privileges.ARTIFACTS_RESOLVE.name());
    }

    public SpringSecurityUser loginUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return null;
        }
        return (SpringSecurityUser) authentication.getPrincipal();
    }

    public String loginUsername() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return "";
        }
        Object o = authentication.getPrincipal();
        if (!(o instanceof SpringSecurityUser)) {
            return "";
        }
        SpringSecurityUser springSecurityUser = (SpringSecurityUser) authentication.getPrincipal();
        if (Objects.isNull(springSecurityUser)) {
            return "";
        }
        return springSecurityUser.getUsername();
    }

    private static boolean setContentLength(HttpServletResponse response) {
        String contentLength = response.getHeader(HttpHeaders.CONTENT_LENGTH);
        if (StringUtils.isBlank(contentLength) || "-1".equalsIgnoreCase(contentLength)) {
            return true;
        }
        return false;
    }

    protected String getBaseUrl(Repository repository) {
        return String.format("%s/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

    protected String getBaseUrl(String storageId, String repositoryId) {
        return String.format("%s/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), storageId, repositoryId);
    }

    protected String getBaseUrlSimple(String storageId, String repositoryId) {
        return String.format("%s/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl().replace("http://", "").replace("https://", ""), "/"), storageId, repositoryId);
    }

    protected String getRepositoryBaseUrl(Repository repository) {
        return String.format("%s/storages/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

    protected String getArtifactoryRepositoryBaseUrl(Repository repository, String endPoint) {
        return String.format("%s/artifactory/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), StringUtils.removeEnd(StringUtils.removeStart(endPoint, GlobalConstants.SEPARATOR), GlobalConstants.SEPARATOR), repository.getId());
    }

    protected String getArtifactoryRepositoryUrl(Repository repository, String endPoint) {
        return String.format("%s/artifactory/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getId());
    }

    /***
     * 获取制品RepositoryPath
     * @param storageId 存储空间名称
     * @param repositoryId 仓库名称
     * @param artifactPath 制品路径
     * @return RepositoryPath
     * @throws Exception 异常
     */
    public Artifact findArtifact(String storageId, String repositoryId, String artifactPath) throws Exception {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        Artifact artifact = Objects.nonNull(repositoryPath) ? repositoryPath.getArtifactEntry() : null;
        if (Objects.isNull(artifact)) {
            //兼容已存在数据的docker布局仓库
            Repository repository = configurationManagementService.getConfiguration().getRepository(storageId, repositoryId);
            if (DockerLayoutProvider.ALIAS.equalsIgnoreCase(repository.getLayout())) {
                //docker
                artifact = findDockerArtifact(artifactPath, storageId, repositoryId);
                return artifact;
            }
        }
        return artifact;
    }

    /**
     * 获取docker Artifact 非镜像版本Artifact信息
     *
     * @param artifactName 制品名称
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @return docker Artifact 非镜像版本Artifact信息
     * @throws IOException 异常
     */
    public Artifact findDockerArtifact(String artifactName, String storageId, String repositoryId) throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactName);
        if (!Files.isDirectory(repositoryPath)) {
            return null;
        }
        DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
        List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerArtifactCoordinates.include(file.getName())).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(fileContents)) {
            return null;
        }
        FileContent fileContent = fileContents.get(0);
        String artifactPath = fileContent.getArtifactPath();
        return artifactRepository.findOneArtifact(storageId, repositoryId, artifactPath);
    }

    public boolean validatePathPrivileges(String storageId, String repositoryId, List<String> paths, String authority) {
        return authComponent.getAllPrivileges(storageId, repositoryId, paths).contains(authority);
    }

    // 根据仓库验证 增加组合仓库过滤
    public boolean validatePathPrivileges(Repository repository, List<String> paths, String authority) {
        if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
            List<String> storageAndRepositoryIds = new LinkedList<>();
            configurationManager.resolveGroupRepository(repository, storageAndRepositoryIds);
            for (String storageAndRepositoryId : storageAndRepositoryIds) {
                String subStorageId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String subRepositoryId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManagementService.getConfiguration().getRepository(subStorageId, subRepositoryId);
                if (RepositoryScopeEnum.OPEN.getType().equals(subRepository.getScope()) || validatePathPrivileges(subRepository, paths, authority)) {
                    return true;
                }
            }
            return false;
        } else {
            return validatePathPrivileges(repository.getStorage().getId(), repository.getId(), paths, authority);
        }
    }

}
