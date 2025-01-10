package com.veadan.folib.controllers.layout.cocoapods;

import cn.hutool.core.io.FileUtil;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.CocoapodsArtifactCoordinates;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.storage.S3FileSystemStorageProvider;
import com.veadan.folib.service.CocoapodsIndexService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.util.CocoapodsArtifactUtil;
import com.veadan.folib.util.RepositoryPathUtil;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/***
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/2 14:58
 * @since x.x.x
 */
@RestController
@LayoutRequestMapping(CocoapodsArtifactCoordinates.LAYOUT_NAME)
public class CocoapodsIndexController
        extends BaseArtifactController {

    private static final String API_ENDPOINT = "/api/pods/";

    @Inject
    private CocoapodsIndexService cocoapodsIndexService;

    @PreAuthorize("authenticated")
    @GetMapping(value = API_ENDPOINT + "{repositoryId}/index/fetchIndex")
    public ResponseEntity repoArtIndex(@RepositoryMapping Repository repository, HttpServletRequest request, HttpServletResponse response) throws Exception {
        final String type = repository.getType();

        ResponseEntity responseEntity = null;
        if (type.equals(RepositoryTypeEnum.HOSTED.getType()))
        { responseEntity = this.repoArtIndexHosted(repository, response); }
        else if (type.equals(RepositoryTypeEnum.PROXY.getType()))
        { responseEntity = this.repoArtIndexProxy(repository, response);}
        else
        { return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("invalid repository type"); }
        if (null != responseEntity)
        { return responseEntity; }

        return new ResponseEntity<>("ok", HttpStatus.OK);
    }
    
    private ResponseEntity repoArtIndexHosted(Repository repository, HttpServletResponse response)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        final RepositoryPath repositoryPath;
        try 
        { repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, ".specs/"); } 
        catch (IOException e) 
        { return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage()); }
        final Path repositoryPathTarget = repositoryPath.getTarget();
        final String baseUrl = super.getBaseUrl();

        ServletOutputStream servletOutputStream = null;
        TarArchiveOutputStream tarArchiveOutputStream = null;
        try
        {
            response.setHeader("Content-Disposition", "attachment;filename=file.tar.gz");
            response.setContentType("application/x-gzip");
            
            final String indexFolder = repositoryPathTarget.toUri().getPath();
            servletOutputStream = response.getOutputStream();
            tarArchiveOutputStream = new TarArchiveOutputStream(new GzipCompressorOutputStream(servletOutputStream));
            tarArchiveOutputStream.setLongFileMode(TarArchiveOutputStream.LONGFILE_GNU);
            if (S3FileSystemStorageProvider.ALIAS.equals(repository.getStorageProvider()))
            {
                final List<S3Path> s3FiePaths = RepositoryPathUtil.getS3FiePaths((S3Path) repositoryPathTarget);
                this.tarGzS3Folder(s3FiePaths, repository, baseUrl, tarArchiveOutputStream);
            }
            else
            { this.tarGzLocalFolder(repository, baseUrl, indexFolder, indexFolder, tarArchiveOutputStream); }
            servletOutputStream.flush();
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
        finally {
            try {
                if (null != tarArchiveOutputStream)
                { tarArchiveOutputStream.close(); }
                if (null != servletOutputStream)
                { servletOutputStream.close(); }
            } catch (Exception e) {
                logger.error("关闭zip文件流失败", e);
            }
        }
        
        return null;
    }

    private ResponseEntity repoArtIndexProxy(Repository repository, HttpServletResponse response) throws Exception 
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        
        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, ".specs/master.tar.gz");
        
        if (!Files.exists(repositoryPath))
        { // 如果未发现索引文件，进行索引同步
            try 
            {
                final boolean syncProxyIndexResult = cocoapodsIndexService.syncProxyIndex(repository);
                logger.info("同步远程仓库（{}）{}", String.format("%s:%s", storageId, repositoryId),syncProxyIndexResult?"成功":"失败");
            }
            catch (Exception e)
            {
                return ResponseEntity
                        .status(HttpStatus.INTERNAL_SERVER_ERROR)
                        .body(e.getMessage());
            }
        }
        else
        { logger.info("仓库（{}）复用已存在索引文件，跳过同步索引逻辑", String.format("%s:%s", storageId, repositoryId)); }

        response.setHeader("Content-Disposition", "attachment;filename=file.tar.gz");
        response.setContentType("application/x-gzip");
        try (final ServletOutputStream outputStream = response.getOutputStream();
             final InputStream fileInputStream = new BufferedInputStream(Files.newInputStream(repositoryPath));
        ){
            int len = 0;
            byte[] buffer = new byte[1024];
            while((len=fileInputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, len);
            }
        }
        
        return ResponseEntity.ok().build();
    }
        
    
    private void tarGzLocalFolder(Repository repository, String baseUrl, String rootPath, String srcFolder, TarArchiveOutputStream archiveOutputStream) throws Exception 
    {
        final File folder = new File(srcFolder);
        //遍历文件夹下所有的文件和文件夹
        final File[] files = folder.listFiles();
        
        if (null != files)
        {
            for (File file : files) 
            {
                //如果是文件夹,递归压缩
                if (file.isDirectory()) {
                    tarGzLocalFolder(repository, baseUrl, rootPath, file.getAbsolutePath(), archiveOutputStream);
                    continue;
                }
    
                if (file.getName().endsWith(".podspec"))
                {
                    byte[] bytes = FileUtil.readBytes(file);
                    final String podspecFileUri = file.getAbsolutePath().replace(rootPath, StringUtils.EMPTY);
                    
                    final RepositoryPath repositoryPath = artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), String.format(".specs/%s", podspecFileUri));
                    if (repositoryPath != null  && null != repositoryPath.getArtifactEntry())
                    { // 获取到有Pod源代码路径地址则替换
                        final Artifact artifactEntry = repositoryPath.getArtifactEntry();
                        CocoapodsArtifactCoordinates cocoapodsArtifactCoordinates = (CocoapodsArtifactCoordinates) artifactEntry.getArtifactCoordinates();
                        URI uri = cocoapodsArtifactCoordinates.convertToResource(cocoapodsArtifactCoordinates);
                        final String newSourceUrl = getArtifactoryRepositoryBaseUrl(repository, API_ENDPOINT) + uri.toString();
                        final String newPodspecContent = CocoapodsArtifactUtil.replaceNewSourceUrlOfPodspecContent(new String(bytes), newSourceUrl);
                        if (StringUtils.isNotBlank(newPodspecContent))
                        { bytes = newPodspecContent.getBytes(StandardCharsets.UTF_8); }
                    }

                    final TarArchiveEntry entry = new TarArchiveEntry(podspecFileUri);
                    entry.setSize(bytes.length);
                    archiveOutputStream.putArchiveEntry(entry);
                    archiveOutputStream.write(bytes);
                    archiveOutputStream.closeArchiveEntry();
                }
            }
        }
    }

    private void tarGzS3Folder(List<S3Path> s3FiePaths, Repository repository, String baseUrl, TarArchiveOutputStream archiveOutputStream) throws Exception
    {
        for (S3Path s3FiePath : s3FiePaths) 
        {
            if (s3FiePath.getFileName().toString().endsWith(".podspec"))
            {
                try (final BufferedInputStream bufferedInputStream = new BufferedInputStream(Files.newInputStream(s3FiePath))) {
                    byte[] bytes = bufferedInputStream.readAllBytes();
                    final String podspecFileUri = s3FiePath.toAbsolutePath().toUri().getPath().replaceAll(".*?\\.specs/(.*?)", "$1");

                    final RepositoryPath repositoryPath = artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), String.format(".specs/%s", podspecFileUri));
                    if (repositoryPath != null  && null != repositoryPath.getArtifactEntry())
                    { // 获取到有Pod源代码路径地址则替换
                        final Artifact artifactEntry = repositoryPath.getArtifactEntry();
                        CocoapodsArtifactCoordinates cocoapodsArtifactCoordinates = (CocoapodsArtifactCoordinates) artifactEntry.getArtifactCoordinates();
                        URI uri = cocoapodsArtifactCoordinates.convertToResource(cocoapodsArtifactCoordinates);
                        final String newSourceUrl = getArtifactoryRepositoryBaseUrl(repository, API_ENDPOINT) + uri.toString();
                        final String newPodspecContent = CocoapodsArtifactUtil.replaceNewSourceUrlOfPodspecContent(new String(bytes), newSourceUrl);
                        if (StringUtils.isNotBlank(newPodspecContent))
                        { bytes = newPodspecContent.getBytes(StandardCharsets.UTF_8); }
                    }

                    final TarArchiveEntry entry = new TarArchiveEntry(podspecFileUri);
                    entry.setSize(bytes.length);
                    archiveOutputStream.putArchiveEntry(entry);
                    archiveOutputStream.write(bytes);
                    archiveOutputStream.closeArchiveEntry();
                }
            }
        }
    }
}
