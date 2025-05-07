package com.veadan.folib.controllers;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.cloud.storage.s3fs.S3FileSystem;
import com.veadan.folib.cloud.storage.s3fs.S3FileSystemProvider;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.cloud.storage.s3fs.util.UriUtils;
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
import com.veadan.folib.providers.io.*;
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
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.carlspring.commons.http.range.ByteRange;
import org.carlspring.commons.http.range.ByteRangeHeaderParser;
import org.carlspring.commons.io.reloading.FSReloadableInputStreamHandler;
import org.eclipse.jetty.io.EofException;
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
import java.io.*;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.FileChannel;
import java.nio.channels.SeekableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.nio.file.spi.FileSystemProvider;
import java.security.NoSuchAlgorithmException;
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
        ArtifactControllerHelper.provideArtifactHeaders(response, repositoryPath, path);
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
        response.setHeader("Content-Disposition", String.format("attachment; filename=\"%s\"", UriUtils.encode(repositoryPath.getFileName().toString())));
        long startTime = System.currentTimeMillis();
        logger.debug("Download [{}] 开始时间 [{}]", repositoryPath.toString(), startTime);
        if (ArtifactControllerHelper.isRangedRequest(httpHeaders)) {
            //分片
            logger.debug("RepositoryPath [{}] Detected ranged request.", path.toString());
            try (FileChannel fileChannel = FileChannel.open(path);
                 WritableByteChannel responseChannel = Channels.newChannel(response.getOutputStream())) {
                // 获取文件总大小
                long fileSize = fileChannel.size();
                String rangeHeader = httpHeaders.getFirst(HttpHeaders.RANGE);
                logger.info("Range header: {}", rangeHeader);
                // 解析范围请求
                ByteRangeHeaderParser parser = new ByteRangeHeaderParser(rangeHeader);
                List<ByteRange> ranges = parser.getRanges();

                // 范围无效处理
                if (CollectionUtils.isEmpty(ranges) || !validateRanges(ranges, fileSize)) {
                    response.setHeader(HttpHeaders.CONTENT_RANGE, "bytes */" + fileSize);
                    response.sendError(HttpStatus.REQUESTED_RANGE_NOT_SATISFIABLE.value());
                    logger.warn("RepositoryPath [{}] Range header is invalid.", path.toString());
                    return false;
                }

                // 只处理第一个范围（多范围需使用 multipart/byteranges）
                ByteRange byteRange = ranges.get(0);
                long start = byteRange.getOffset();
                Long end = byteRange.getLimit();
                if (Objects.isNull(end) || end >= fileSize) {
                    end = fileSize - 1;
                }
                if (end < 0) {
                    // 处理负数，转换为绝对值
                    end = Math.abs(end);
                }
                // 范围有效性二次验证
                if (start < 0 || end < 0 || start > end || start >= fileSize || end >= fileSize) {
                    response.setHeader(HttpHeaders.CONTENT_RANGE, "bytes */" + fileSize);
                    response.sendError(HttpStatus.REQUESTED_RANGE_NOT_SATISFIABLE.value());
                    logger.warn("RepositoryPath [{}] Range header is invalid.", path.toString());
                    return false;
                }
                logger.info("Range start:[{}] end:[{}]  fileSize:[{}]",start,end,fileSize);
                // 设置响应头
                long contentLength = end - start + 1; // 注意字节数计算
                logger.info("Length:[{}]",contentLength);
                response.setStatus(HttpStatus.PARTIAL_CONTENT.value());
                response.setHeader(HttpHeaders.CONTENT_RANGE, String.format("bytes %d-%d/%d", start, end, fileSize));
                response.setHeader(HttpHeaders.CONTENT_LENGTH, String.valueOf(contentLength));

                // 传输数据
                long transferred = fileChannel.transferTo(start, contentLength, responseChannel);
                logger.info("Transferred {} bytes (range {}-{})", transferred, start, end);

            } catch (Exception e) {
                logger.error("File transfer error", e);
                throw e;
            }
        } else if (path.toString().startsWith("s3://")) {
            //S3
            if (path instanceof RepositoryPath) {
                if (!Files.exists(path)) {
                    throw new ArtifactNotFoundException(path.toUri());
                }
                if (Files.isDirectory(path)) {
                    throw new ArtifactNotFoundException(path.toUri(),
                            String.format("The artifact path is a directory: [%s]", path.toString()));
                }
                StorageFileSystemProvider storageFileSystemProvider = (StorageFileSystemProvider) path.getFileSystem().provider();
                S3FileSystemProvider s3FileSystemProvider =  (S3FileSystemProvider) storageFileSystemProvider.getTarget();
                Path s3Path = unwrap(path);
                try (InputStream s3FileSystem = s3FileSystemProvider.newInputStream(s3Path);) {
                    copyToResponse(s3FileSystem, response);
                }
                //try (InputStream is = artifactResolutionService.getInputStream((RepositoryPath) path)) {
                //    copyToResponse(is, response);
                //}
            }
        } else {
            try (FileChannel fileChannel = FileChannel.open(path);
                 WritableByteChannel responseChannel = Channels.newChannel(response.getOutputStream())) {
                long fileSize = fileChannel.size();
                for (long left = fileSize; left > 0; ) {
                    try {
                        logger.debug("RepositoryPath [{}] position [{}] left [{}]", path.toString(), fileSize - left, left);
                        left -= fileChannel.transferTo((fileSize - left), left, responseChannel);
                    } catch (IOException e){
                        // 检查是否为客户端断开连接
                        if (e instanceof ClosedChannelException || e instanceof EofException) {
                            logger.error("Client disconnected, stopping transfer userAgent [{}] path [{}] error [{}]", request.getHeader("User-Agent"), path, ExceptionUtils.getStackTrace(e));
                        }
                        //其他IO异常重新抛出
                        throw e;
                    }
                }
            }
        }
        artifactComponent.afterRead(repositoryPath);
        logger.debug("Download [{}] 结束时间 [{}]", repositoryPath.toString(), System.currentTimeMillis() - startTime);
        return true;
    }

    private boolean validateRanges(List<ByteRange> ranges, long fileSize) {
        return ranges.stream().allMatch(range -> {
            if (range == null) {
                return false;
            }
            Long start = range.getOffset();
            Long end = range.getLimit();
            if (Objects.isNull(end) || end >= fileSize) {
                end = fileSize - 1;
            }
            if (end < 0) {
                // 处理负数，转换为绝对值
                end = Math.abs(end);
            }
            // 确保范围有效
            return start >= 0 &&
                    end >= start &&
                    end < fileSize;
        });
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

    protected Path unwrap(Path path) {
        return path instanceof RepositoryPath ? ((RepositoryPath) path).getTarget() : path;
    }

}
