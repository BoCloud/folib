package com.veadan.folib.controllers;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.components.ArtifactSecurityComponent;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.block.ArtifactBlockComponent;
import com.veadan.folib.controllers.support.ErrorResponseEntityBody;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.CacheSettings;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.enums.ProductTypeEnum;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.io.ByteRangeInputStream;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.services.DirectoryListingService;
import com.veadan.folib.storage.metadata.MetadataHelper;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.util.CacheUtil;
import com.veadan.folib.utils.ArtifactControllerHelper;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.carlspring.commons.http.range.ByteRange;
import org.carlspring.commons.http.range.ByteRangeHeaderParser;
import org.carlspring.commons.io.reloading.FSReloadableInputStreamHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.*;
import org.springframework.ui.ModelMap;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.HandlerMapping;
import org.springframework.web.servlet.ModelAndView;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.text.SimpleDateFormat;
import java.util.*;

import static org.springframework.http.HttpStatus.PARTIAL_CONTENT;

public abstract class BaseArtifactController
        extends BaseController {

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Autowired
    private HttpServletResponse httpServletResponse;

    @Autowired
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Autowired
    private ArtifactComponent artifactComponent;

    @Autowired
    private ArtifactBlockComponent artifactBlockComponent;

    @Autowired
    private DictService dictService;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    @Lazy
    private volatile DirectoryListingService directoryListingService;

    @Autowired
    @Lazy
    private ArtifactSecurityComponent artifactSecurityComponent;


    protected boolean provideArtifactDownloadResponse(HttpServletRequest request,
                                                      HttpServletResponse response,
                                                      HttpHeaders httpHeaders,
                                                      RepositoryPath repositoryPath)
            throws Exception {

        // If the response is already committed, there's no need to proceed.
        if (response.isCommitted()) {
            return false;
        }
        Path path = getCachePath(repositoryPath);
        ArtifactControllerHelper.provideArtifactHeaders(response, path);
        // If the resource is not found, return false.
        if (response.getStatus() == HttpStatus.NOT_FOUND.value()) {
            return false;
        }
        // If it's a HEAD request, return true.
        if (RequestMethod.HEAD.name().equals(request.getMethod())) {
            return true;
        }
        SimpleDateFormat sdf = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH);
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        response.setHeader("Last-Modified", sdf.format(new Date()));
        response.setHeader("Content-Disposition", String.format("attachment; filename=\"%s\"", repositoryPath.getFileName()));
        long startTime = System.currentTimeMillis();
        logger.debug("Download [{}] 开始时间 [{}]", repositoryPath.toString(), startTime);
        if (ArtifactControllerHelper.isRangedRequest(httpHeaders)) {
            //分片
            logger.debug("RepositoryPath [{}] Detected ranged request.", path.toString());
            //try (ByteRangeInputStream is = new ByteRangeInputStream(Files.newInputStream(path))) {
            //    //is.setReloadableInputStreamHandler(new FSReloadableInputStreamHandler(path));
            //    is.setLength(Files.size(path));
            //    ArtifactControllerHelper.handlePartialDownload(is, httpHeaders, response);
            //}
            try (FileChannel fileChannel = FileChannel.open(path);
                 WritableByteChannel responseChannel = Channels.newChannel(response.getOutputStream())) {
                String contentRange = httpHeaders.getFirst(HttpHeaders.RANGE);
                ByteRangeHeaderParser parser = new ByteRangeHeaderParser(contentRange);
                List<ByteRange> ranges = parser.getRanges();
                if (!CollectionUtils.isEmpty(ranges)) {
                    //long fileSize = fileChannel.size();
                    ByteRange byteRange = ranges.get(0);
                    response.setHeader(HttpHeaders.CONTENT_RANGE, String.format("bytes %d-%d/%d", byteRange.getOffset(), byteRange.getLimit(), byteRange.getLimit()));
                    response.setHeader(HttpHeaders.CONTENT_LENGTH, String.valueOf(byteRange.getLimit() - byteRange.getOffset()));
                    response.setStatus(PARTIAL_CONTENT.value());
                    logger.debug("ByteRange: offset={}, limit={}", byteRange.getOffset(), byteRange.getLimit());
                    logger.debug("Starting file transfer from position {}", byteRange.getOffset());
                    long transferred = fileChannel.transferTo(byteRange.getOffset(), byteRange.getLimit(), responseChannel);
                    logger.debug("Transferred {} bytes", transferred);
                }
            }
        } else if (path.toString().startsWith("s3://")) {
            //S3
            if (path instanceof RepositoryPath) {
                try (InputStream is = artifactResolutionService.getInputStream((RepositoryPath) path)) {
                    copyToResponse(is, response);
                }
            }
        } else {
            try (FileChannel fileChannel = FileChannel.open(path);
                 WritableByteChannel responseChannel = Channels.newChannel(response.getOutputStream())) {
                long fileSize = fileChannel.size();
                for (long left = fileSize; left > 0; ) {
                    logger.debug("RepositoryPath [{}] position [{}] left [{}]", path.toString(), fileSize - left, left);
                    left -= fileChannel.transferTo((fileSize - left), left, responseChannel);
                }
            }
        }
        artifactComponent.afterRead(repositoryPath);
        logger.debug("Download [{}] 结束时间 [{}]", repositoryPath.toString(), System.currentTimeMillis() - startTime);
        return true;
    }

    public ResponseEntity<String> checkRepositoryAccess() {
        return new ResponseEntity<>("success", HttpStatus.OK);
    }

    /**
     * 漏洞阻断下载
     *
     * @param repositoryPath 制品信息
     * @throws IOException io异常
     */
    public Artifact vulnerabilityBlock(RepositoryPath repositoryPath) throws IOException {
        boolean supportLayout = artifactComponent.layoutSupportsForBlock(repositoryPath);
        if (!supportLayout) {
            return null;
        }
        Artifact artifact = getArtifact(repositoryPath);
        if (Objects.isNull(artifact)) {
            return null;
        }
        boolean block = artifactBlockComponent.artifactBlockStrategy(artifact, repositoryPath.getRepository().getLayout());
        if (block) {
            httpServletResponse.setContentType(org.springframework.http.MediaType.APPLICATION_JSON_VALUE);
            httpServletResponse.setStatus(HttpServletResponse.SC_FORBIDDEN);
            String msg = "The artifact " + artifact.getUuid() + " has a vulnerability, and downloading is prohibited";
            httpServletResponse.getWriter().println(objectMapper.writeValueAsString(new ErrorResponseEntityBody(msg)));
            httpServletResponse.flushBuffer();
            artifactEventListenerRegistry.dispatchArtifactDownloadBlockedEvent(repositoryPath);
        }
        return artifact;
    }

    protected String getBaseUrl() {
        return StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/");
    }

    @Override
    protected String getBaseUrl(Repository repository) {
        return String.format("%s/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

    public boolean artifactRealExists(RepositoryPath repositoryPath) {
        try {
            if (Objects.isNull(repositoryPath)) {
                return false;
            }
            repositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
            return Files.exists(repositoryPath) && Objects.nonNull(repositoryPath.getArtifactEntry());
        } catch (Exception ex) {
            logger.error("判断制品是否存在发生错误：{}", ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    private Path getCachePath(RepositoryPath repositoryPath) {
        if (Objects.isNull(repositoryPath)) {
            return null;
        }
        Path path = repositoryPath;
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
        try {
            CacheSettings cacheSettings = artifactComponent.getCacheConfig();
            if (Objects.isNull(cacheSettings) || !cacheSettings.isEnabled()) {
                return path;
            }
            Path cacheParentPath = Files.createDirectories(Paths.get(cacheSettings.getDirectoryPath()));
            String sourcePath = repositoryPath.toString();
            String prefix = String.format("/%s/%s/", storageId, repositoryId);
            String targetSubPath = sourcePath.substring(sourcePath.indexOf(prefix) + 1);
            Path targetPath = cacheParentPath.resolve(targetSubPath);
            boolean existsCache = Files.exists(targetPath) && (RepositoryFiles.isArtifactChecksum(FilenameUtils.getName(targetPath.getFileName().toString())) || RepositoryFiles.validateChecksum(repositoryPath, targetPath) || DockerLayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout()));
            if (existsCache) {
                logger.info("存在缓存 storageId [{}] repositoryId [{}]，源制品地址 [{}] 缓存制品地址 [{}]", storageId, repositoryId, sourcePath, targetPath.toString());
                path = targetPath;
//                artifactComponent.asyncHandlerArtifactCacheRecord(repositoryPath, cacheSettings, targetPath);
            } else {
                //不存在缓存，触发缓存事件
                if (repositoryPath.toString().contains(MetadataHelper.MAVEN_METADATA_XML)) {
                    return path;
                }
                artifactComponent.artifactCache(repositoryPath);
            }
        } catch (Exception ex) {
            logger.warn("缓存制品 [{}] [{}] [{}] 错误 [{}]", storageId, repositoryId, repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
        }
        return path;
    }

    private Artifact getArtifact(RepositoryPath repositoryPath) throws IOException {
        Artifact artifact = null;
        if (Files.isSameFile(repositoryPath.getRoot(), repositoryPath)) {
            return null;
        }
        String fileName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + ".metadata";
        if (repositoryPath.getTarget() instanceof S3Path) {
            CacheUtil<String, String> cacheUtil = CacheUtil.getInstance();
            String cacheRootPathDir = cacheUtil.get("ARTIFACT_CACHE_ROOT_PATH");
            if (StringUtils.isNotBlank(cacheRootPathDir)) {
                Path cacheRootPath = Path.of(cacheRootPathDir);
                String sourcePath = repositoryPath.toString();
                String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
                String prefix = String.format("/%s/%s/", storageId, repositoryId);
                String targetSubPath = sourcePath.substring(sourcePath.indexOf(prefix) + 1);
                Path cacheArtifactPath = cacheRootPath.resolve(targetSubPath);
                Path cacheArtifactMetadataPath = cacheArtifactPath.getParent().resolve(fileName);
                if (Files.exists(cacheArtifactMetadataPath)) {
                    //获取metadata缓存文件
                    artifact = parseArtifact(cacheArtifactMetadataPath);
                }
            }
        }
        if (Objects.isNull(artifact)) {
            RepositoryPath artifactMetadataRepositoryPath = repositoryPath.getParent().resolve(fileName);
            if (Files.exists(artifactMetadataRepositoryPath)) {
                //获取metadata源文件
                artifact = parseArtifact(artifactMetadataRepositoryPath);
            }
        }
        if (Objects.isNull(artifact)) {
            //查询图库
            try {
                artifact = repositoryPath.getArtifactEntry();
                artifactComponent.storeArtifactMetadataFile(repositoryPath);
            } catch (Exception ex) {
                logger.warn("查询制品信息 [{}] 错误 [{}]", repositoryPath, ExceptionUtils.getStackTrace(ex));
            }
        }
        return artifact;
    }

    private Artifact parseArtifact(Path path) {
        Artifact artifact = null;
        try (InputStream inputStream = Files.newInputStream(path);
             ObjectInputStream objectInputStream = new ObjectInputStream(inputStream)) {
            artifact = (Artifact) objectInputStream.readObject();
        } catch (Exception ex) {
            try {
                Files.deleteIfExists(path);
            } catch (Exception e) {

            }
            logger.warn("解析制品 [{}] 本地缓存.metadata文件错误 [{}]", path, ExceptionUtils.getStackTrace(ex));
        }
        return artifact;
    }

    /**
     * 提取请求路径中为/**的内容
     *
     * @param request 请求
     * @return 提取请求路径中为/**的内容
     */
    protected String getExtractPath(final HttpServletRequest request) {
        String path = (String) request.getAttribute(HandlerMapping.PATH_WITHIN_HANDLER_MAPPING_ATTRIBUTE);
        String bestMatchPattern = (String) request.getAttribute(HandlerMapping.BEST_MATCHING_PATTERN_ATTRIBUTE);
        return new AntPathMatcher().extractPathWithinPattern(bestMatchPattern, path);
    }

    protected boolean probeForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException {
        return Files.exists(repositoryPath) &&
                repositoryPath.getRepository().getLayout().equals("helm") && repositoryPath.getTarget().toString().endsWith("index.yaml") || Files.isDirectory(repositoryPath) &&
                isPermittedForDirectoryListing(repositoryPath);
    }

    protected boolean isPermittedForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException {
        //TODO: RepositoryFiles.isIndex(repositoryPath) || (
        return (!Files.isHidden(repositoryPath)
                // 支持Cocoapods索引目录的显示
                || repositoryPath.toString().contains(".specs") || repositoryPath.toString().contains(LayoutFileSystem.TRASH))
                && !RepositoryFiles.isTemp(repositoryPath);
    }

    public Object browseRepository(HttpServletRequest request, HttpHeaders httpHeaders, HttpServletResponse response, ModelMap model, Repository repository, String path) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested browsing repository content at {}/{}/{} ", storageId, repositoryId, path);
        String acceptHeader = request.getHeader(HttpHeaders.ACCEPT);
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, path);
            repositoryPath.setDisableRemote(true);
            RepositoryPath repositoryResolvePath = artifactResolutionService.resolvePath(repositoryPath);
            if (Objects.isNull(repositoryResolvePath) && StringUtils.isNotBlank(RepositoryFiles.relativizePath(repositoryPath))) {
                response.setStatus(HttpStatus.NOT_FOUND.value());
                return null;
            }
            if (Objects.nonNull(repositoryResolvePath) && Files.exists(repositoryResolvePath) && Files.isRegularFile(repositoryResolvePath)) {
                vulnerabilityBlock(repositoryResolvePath);
                provideArtifactDownloadResponse(request, response, httpHeaders, repositoryResolvePath);
                return null;
            }
            DirectoryListing directoryListing = null;
            if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                directoryListing = directoryListingService.fromGroupRepositoryPath(repository, repositoryPath);
            } else {
                if (repositoryPath == null || !Files.exists(repositoryPath)) {
                    return getNotFoundResponseEntity("The requested repository path was not found.", acceptHeader);
                }
                if (!repository.isInService()) {
                    return getServiceUnavailableResponseEntity("Repository is not in service...", acceptHeader);
                }
                if (!repository.isAllowsDirectoryBrowsing() || !probeForDirectoryListing(repositoryPath)) {
                    return getNotFoundResponseEntity("Requested repository doesn't allow browsing.", acceptHeader);
                }
                directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
            }
            if (acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE)) {
                return ResponseEntity.ok(objectMapper.writer().writeValueAsString(directoryListing));
            }
            String currentUrl = org.apache.commons.lang.StringUtils.chomp(request.getRequestURI(), "/");
            model.addAttribute("currentUrl", currentUrl);
            model.addAttribute("directories", directoryListing.getDirectories());
            model.addAttribute("files", directoryListing.getFiles());
            return new ModelAndView("directoryListing", model);
        } catch (Exception e) {
            String message = "Failed to generate repository directory listing.";
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, message, e, acceptHeader);
        }
    }

    public Object download(Repository repository, HttpHeaders httpHeaders, String path, HttpServletRequest request, HttpServletResponse response, ModelMap model)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, path);
        repositoryPath.setDisableRemote(true);
        if (ProductTypeEnum.SIMPLE_TYPE_LIST.stream().anyMatch(item -> item.equals(repository.getLayout()))) {
            repositoryPath.setDisableRemote(null);
        }
        if (StringUtils.isNotBlank(RepositoryFiles.relativizePath(repositoryPath)) && !RepositoryTypeEnum.GROUP.getType().equals(repository.getType()) && !artifactSecurityComponent.validatePrivileges(repositoryPath, Privileges.ARTIFACTS_RESOLVE.getAuthority())) {
            response.setStatus(HttpStatus.NOT_FOUND.value());
            return null;
        }
        repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
        boolean browse = RepositoryTypeEnum.GROUP.getType().equals(repository.getType()) || (Objects.nonNull(repositoryPath) && Files.exists(repositoryPath) && Files.isDirectory(repositoryPath));
        if (browse) {
            return browseRepository(request, httpHeaders, response, model, repository, path);
        } else {
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        }
        return null;
    }

    /**
     * 获取设置默认的存储空间
     *
     * @param repositoryId 仓库名称
     * @return 存储空间
     */
    public String getDefaultStorageId(String repositoryId) {
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        if (StringUtils.isNotBlank(repositoryId)) {
            //按照仓库查询对应的存储空间
            String key = "JFrogAdapterStorage_" + repositoryId;
            String jFrogAdapterStorage = distributedCacheComponent.get(key);
            if (StringUtils.isNotBlank(jFrogAdapterStorage)) {
                return jFrogAdapterStorage;
            }
        }
        String key = "JFrogAdapterDefaultStorage";
        String jFrogAdapterDefaultStorage = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(jFrogAdapterDefaultStorage)) {
            throw new RuntimeException("Default storage not found,Please Set the default storageId");
        }
        return jFrogAdapterDefaultStorage;
    }

}
