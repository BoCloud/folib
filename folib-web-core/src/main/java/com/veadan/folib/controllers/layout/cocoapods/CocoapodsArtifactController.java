package com.veadan.folib.controllers.layout.cocoapods;

import cn.hutool.core.io.FileUtil;
import com.veadan.folib.artifact.coordinates.CocoapodsArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.util.CocoapodsArtifactUtil;
import com.veadan.folib.utils.CompressUtil;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/3 15:29
 * @since x.x.x
 */
@RestController
@LayoutRequestMapping(CocoapodsArtifactCoordinates.LAYOUT_NAME)
public class CocoapodsArtifactController extends BaseArtifactController
{
    @Value("${folib.temp}")
    private String tempPath;
    
    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity uploadPod(@RepositoryMapping Repository repository,
                                 @PathVariable String artifactPath,
                                 HttpServletRequest request) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try {
            final BufferedInputStream artifactByteArrayInputStream = new BufferedInputStream(request.getInputStream());
            final byte[] cacheBytes = artifactByteArrayInputStream.readAllBytes();
            // 兼容网络路径
            final String podspecSourceContent = CocoapodsArtifactUtil.fetchPodspecSourceContentByInputStream(new ByteArrayInputStream(cacheBytes));

            // 读取pod.tar.gz中的*.podspec文件内容
            if (StringUtils.isNotBlank(podspecSourceContent))
            {
                final CocoapodsArtifactUtil.PodSpec podSpec = CocoapodsArtifactUtil.resolvePodSpec(podspecSourceContent);
                // 存储制品文件
                final RepositoryPath podRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                final ArtifactEntity podArtifactEntity = new ArtifactEntity(storageId, repositoryId, RepositoryFiles.readCoordinates(podRepositoryPath));
                final CocoapodsArtifactCoordinates podArtifactCoordinates = (CocoapodsArtifactCoordinates) podArtifactEntity.getArtifactCoordinates();
                podArtifactCoordinates.setBaseName(podSpec.getName());
                podArtifactCoordinates.setVersion(podSpec.getVersion());
                podRepositoryPath.setArtifact(podArtifactEntity);
                artifactManagementService.validateAndStore(podRepositoryPath, new ByteArrayInputStream(cacheBytes));
                
                // 存储索引文件
                final ByteArrayInputStream podspecContentByteArrayInputStream = new ByteArrayInputStream(podspecSourceContent.getBytes(StandardCharsets.UTF_8));
                final String uri = podRepositoryPath.toUri().getPath();
                final RepositoryPath podSpecRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, String.format(".specs/%s/%s/%s.podspec", podSpec.getName(), podSpec.getVersion(), podSpec.getName()));
                final ArtifactEntity podSpecArtifactEntity = new ArtifactEntity(storageId, repositoryId, RepositoryFiles.readCoordinates(podSpecRepositoryPath));
                final CocoapodsArtifactCoordinates artifactCoordinates = (CocoapodsArtifactCoordinates) podSpecArtifactEntity.getArtifactCoordinates();
                artifactCoordinates.setPath(uri);
                artifactCoordinates.setBaseName(podSpec.getName());
                artifactCoordinates.setVersion(podSpec.getVersion());
                podSpecRepositoryPath.setArtifact(podSpecArtifactEntity);
                artifactManagementService.validateAndStore(podSpecRepositoryPath, podspecContentByteArrayInputStream);
            }

            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = { @ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = { "{storageId}/{repositoryId}/{path:.+}" })
    public void download(@RepositoryMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable String path,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception
    {
        final String type = repository.getType();
        if (type.equals(RepositoryTypeEnum.HOSTED.getType()))
        { this.downloadHosted(repository, httpHeaders, path, request, response); }
        else if (type.equals(RepositoryTypeEnum.PROXY.getType()))
        { this.downloadProxy(repository, httpHeaders, path, request, response);}
    }


    private void downloadHosted(Repository repository,
                                HttpHeaders httpHeaders,
                                String path,
                                HttpServletRequest request,
                                HttpServletResponse response) throws Exception 
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }
    
    private void downloadProxy(Repository repository,
                               HttpHeaders httpHeaders,
                               String path,
                               HttpServletRequest request,
                               HttpServletResponse response) throws Exception 
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

//        final RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        final Pattern compile = Pattern.compile("pod/git/(.*?)/(.*?)/(.*?)$");
        final Matcher matcher = compile.matcher(path);
        if (!matcher.find()) 
        { 
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, path);
    
            if (Files.exists(repositoryPath))
            { // 如果已经缓存过，则直接返回下载
                vulnerabilityBlock(repositoryPath);
                provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
            }
            response.setStatus(HttpStatus.NOT_FOUND.value());
            return;
        }
        
        final String owner = matcher.group(1);
        final String podName = matcher.group(2);
        final String version = matcher.group(3);
        final String artifactCacheFolderPath = String.format("%s/%s/tags/%s/temp", owner, podName, version);

        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);
        final String targetUrl = String.format("https://github.com/%s/%s/archive/refs/tags/%s.zip", owner, podName, version),
                artifactZipCachePath = String.format("%s/%s-%s.zip", artifactCacheFolderPath, podName, version), 
                artifactTarGzPath = String.format("%s/%s/tags/%s/%s-%s.tar.gz", owner, podName, version, podName, version);

//        RepositoryPath repositoryTarGzPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactTarGzPath);
        RepositoryPath repositoryTarGzPath = repositoryPathResolver.resolve(repository, artifactTarGzPath);
        downloadNewPod:
        if (null != repositoryTarGzPath && FileUtil.exist(repositoryTarGzPath.toString()))
        { // 在repo-art插件请求下载前判断是否存在，存在则直接返回
            vulnerabilityBlock(repositoryTarGzPath);
            response.setHeader("Content-Disposition", String.format("attachment;filename=%s-%s.tar.gz", podName, version));
            response.setContentType("application/x-gzip");
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryTarGzPath);
            return;
        }

        RepositoryPath artifactZipCacheRPath = null;
        final String artifact2TarGzLocalTempPath = String.format("%s/%s.zip", tempPath, UUID.randomUUID());
        try {
            artifactZipCacheRPath = artifactResolutionService.resolvePath(storageId, repositoryId, targetUrl, artifactZipCachePath);
            final BufferedInputStream zipInputStream = new BufferedInputStream(Files.newInputStream(artifactZipCacheRPath));
            // 转存到本地临时文件
            CompressUtil.zipInputSteam2TarGzFile(zipInputStream, artifact2TarGzLocalTempPath);

            repositoryTarGzPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactTarGzPath);
            artifactManagementService.store(repositoryTarGzPath, Files.newInputStream(Path.of(artifact2TarGzLocalTempPath)));
            artifactManagementService.validateAndStoreIndex(repositoryTarGzPath);
            
            // 装在附加信息
            final ArtifactEntity artifactEntity = new ArtifactEntity(storageId, repositoryId, RepositoryFiles.readCoordinates(repositoryTarGzPath));
            final CocoapodsArtifactCoordinates artifactCoordinates = (CocoapodsArtifactCoordinates) artifactEntity.getArtifactCoordinates();
            artifactCoordinates.setVersion(version);
            artifactCoordinates.setBaseName(podName);
            repositoryTarGzPath.setArtifact(artifactEntity);
            artifactManagementService.validateAndStoreIndex(repositoryTarGzPath);
            
            vulnerabilityBlock(repositoryTarGzPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryTarGzPath);
        }
        finally
        {
            // 删除临时目录
            if (null != artifactZipCacheRPath)
            { artifactManagementService.delete(artifactZipCacheRPath.getParent(), true); }
            final File artifact2TarGzLocalTempFile = new File(artifact2TarGzLocalTempPath);
            if (FileUtil.exist(artifact2TarGzLocalTempFile))
            { FileUtil.del(artifact2TarGzLocalTempFile); }
        }
    }
}
