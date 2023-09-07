package com.veadan.folib.controllers.layout.cocoapods;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.artifact.coordinates.CocoapodsArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.util.CocoapodsArtifactUtil;
import com.veadan.folib.utils.CompressUtil;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

    //    @PreAuthorize("hasAuthority('MANAGEMENT_REBUILD_INDEXES')")
    @GetMapping(value = "/{storageId}/{repositoryId}/index/fetchIndex")
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
            this.tarGzFolder(repository, baseUrl, indexFolder, indexFolder, tarArchiveOutputStream);
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

    private static final Pattern POD_REPO_GIT_URL_PATTERN = Pattern.compile("http(?:s)?://.*?/(.*?)/(.*?)\\.git");
    
    private final AtomicBoolean DOWNLOAD_COCOAPODS_PROXY_INDEX_LOCK = new AtomicBoolean(false);
    
    private ResponseEntity repoArtIndexProxy(Repository repository, HttpServletResponse response) throws Exception {
        if (DOWNLOAD_COCOAPODS_PROXY_INDEX_LOCK.get())
        {
            return ResponseEntity
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("目前代理正在缓存中，请稍后再试");
        }

        DOWNLOAD_COCOAPODS_PROXY_INDEX_LOCK.set(true);
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        final RemoteRepository remoteRepository = repository.getRemoteRepository();
        String url = remoteRepository.getUrl();
        final String baseUrl = super.getBaseUrl();
        final String username = remoteRepository.getUsername();
        final String password = remoteRepository.getPassword();
        url = String.format("%s/archive/refs/heads/master.zip", url);
        final String specIndexZipTempUri = ".specs/temp/master.zip";
        RepositoryPath specIndexZipTempPath = null;

        try {
            // 下载代理索引zip
            specIndexZipTempPath = artifactResolutionService.resolvePath(storageId, repositoryId, url, specIndexZipTempUri);
//            final RepositoryPath specIndexZipTempPath = artifactResolutionService.resolvePath(storageId, repositoryId, specIndexZipTempUri);
            logger.info("开始转换Cocoapods仓库代理仓库Zip（{}）", specIndexZipTempUri);
            final JSONObject podNewSourceObj = new JSONObject();
            CompressUtil.zip2Targz(specIndexZipTempPath.getTarget().toString(), specIndexZipTempPath.getTarget().getParent().getParent().toString()+"/master.tar.gz",
                    (zipEntryName -> zipEntryName.matches(".*?/.{1}/.{1}/.{1}/(.*)")),
                    (zipEntryName -> zipEntryName.replaceAll(".*?/.{1}/.{1}/.{1}/(.*)", "Specs/$1")),
                    (zipEntryName -> zipEntryName.endsWith(".podspec.json")),
                    ((zipEntryName, extra) ->
                    {
                        // 将资源下载指向folib
                        final String podSpecJson = new String(extra, StandardCharsets.UTF_8);
                        try {
                            JSONObject podJsonObj = null;
                            try {
                                podJsonObj = JSON.parseObject(podSpecJson);
                            } catch (Exception e) {
                                return extra;
                            }
                            final JSONObject sourceObj = podJsonObj.getJSONObject("source");
                            if (null != sourceObj && sourceObj.containsKey("git") && sourceObj.containsKey("tag")) {
                                final String podRepoGitUrl = sourceObj.getString("git");
                                final String version = sourceObj.getString("tag");
                                final Matcher podRepoGitUrlMatcher = POD_REPO_GIT_URL_PATTERN.matcher(podRepoGitUrl);
                                if (podRepoGitUrlMatcher.find()) {
                                    final String owner = podRepoGitUrlMatcher.group(1);
                                    final String podName = podRepoGitUrlMatcher.group(2);
                                    final String newSourceUrl = String.format("%s/storages/%s/%s/pod/git/%s/%s/%s", baseUrl, storageId, repositoryId, owner, podName, version);
                                    podNewSourceObj.clear();
                                    podNewSourceObj.put("http", newSourceUrl);
                                    podNewSourceObj.put("type", "tgz");
                                    podJsonObj.put("source", podNewSourceObj);
                                    return JSON.toJSONString(podJsonObj, true).getBytes(StandardCharsets.UTF_8);
                                } else { /*logger.info("非法PodGitUrl：{}", podRepoGitUrl);*/ }
                            } else { /*logger.info("非法PodSource信息：{}", JSON.toJSONString(sourceObj));*/ }
                        }catch (Exception e)
                        {
                            logger.info("编码错误PodSpecJson文件：{}", zipEntryName);
                        }
                        return extra;
                    }));
            logger.info("结束转换Cocoapods仓库代理仓库Zip（{}）", specIndexZipTempUri);

        } 
        catch (Exception e) 
        {
            e.printStackTrace();
            logger.info("下载Cocoapods远程索引失败");
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
        finally 
        {
            DOWNLOAD_COCOAPODS_PROXY_INDEX_LOCK.set(false);
            if (null != specIndexZipTempPath)
            { artifactManagementService.delete(specIndexZipTempPath.getParent(), true); }
        }

        response.setHeader("Content-Disposition", "attachment;filename=file.tar.gz");
        response.setContentType("application/x-gzip");
        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, ".specs/master.tar.gz");
        final File proxyTarGzFile = repositoryPath.getTarget().toFile();
        try (final ServletOutputStream outputStream = response.getOutputStream();
             final FileInputStream fileInputStream = new FileInputStream(proxyTarGzFile);
        ){
            int len = 0;
            byte[] buffer = new byte[1024];
            while((len=fileInputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, len);
            }
        }
        
        return ResponseEntity.ok("代理缓存任务已经成功提交，请稍后进行拉取代理仓库");
    }

    
    private void tarGzFolder(Repository repository, String baseUrl, String rootPath, String srcFolder, TarArchiveOutputStream archiveOutputStream) throws Exception 
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
                    tarGzFolder(repository, baseUrl, rootPath, file.getAbsolutePath(), archiveOutputStream);
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
                        final String path = artifactEntry.getArtifactCoordinates().getPath();
                        final String newSourceUrl = String.format("%s/%s%s", baseUrl, "storages", path);
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
