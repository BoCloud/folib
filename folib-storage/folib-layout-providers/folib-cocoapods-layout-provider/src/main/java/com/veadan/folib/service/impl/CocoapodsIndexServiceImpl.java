package com.veadan.folib.service.impl;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.storage.S3FileSystemStorageProvider;
import com.veadan.folib.service.CocoapodsIndexService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.util.CompressUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author veadan
 * @date 2023/9/21 15:24
 */
@Component
public class CocoapodsIndexServiceImpl implements CocoapodsIndexService
{
    
    protected final Logger logger = LoggerFactory.getLogger(CocoapodsIndexServiceImpl.class);
    private static final Pattern POD_REPO_GIT_URL_PATTERN = Pattern.compile("http(?:s)?://.*?/(.*?)/(.*?)\\.git");
    private final Map<String, Boolean> SYNC_COCOAPODS_PROXY_INDEX_LOCK_MAP = new ConcurrentHashMap<>();
    
    @Value("${folib.temp}")
    private String tempPath;

    @Inject
    protected ArtifactManagementService artifactManagementService;
    @Inject
    protected ArtifactResolutionService artifactResolutionService;
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;
    @Inject
    protected ConfigurationManager configurationManager;
    
    
    @Override
    public boolean syncProxyIndex(Repository repository) 
    {
        final String repositorySyncLockKey = String.format("%s:%s", repository.getStorage().getId(), repository.getId());
        if (SYNC_COCOAPODS_PROXY_INDEX_LOCK_MAP.getOrDefault(repositorySyncLockKey, false))
        { throw new RuntimeException("目前代理正在缓存中，请稍后再试"); }

        SYNC_COCOAPODS_PROXY_INDEX_LOCK_MAP.put(repositorySyncLockKey, true);
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        final RemoteRepository remoteRepository = repository.getRemoteRepository();
        String url = remoteRepository.getUrl();
        final String baseUrl = StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/");
        final String username = remoteRepository.getUsername();
        final String password = remoteRepository.getPassword();
        String url2 = String.format("%s/archive/refs/heads/master.tar.gz", url);
        url = String.format("%s/archive/refs/heads/master.zip", url);

        final String specIndexZipTempUri = ".specs/temp/master.zip";
        final String specIndexTarGzTempUri = ".specs/master.tar.gz";
        final String indexTempFolderPath = String.format("%s%s%s%s", tempPath, File.separator, UUID.randomUUID(), File.separator);
        RepositoryPath specIndexZipTempPath = null;
        String ziFilePath = null;

        String URL_PATTERN = "^(http|https)://(?:[a-zA-Z0-9.-]+|\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3})(?::\\d+)?/storages/[^/]+/[^/]+$";

        try 
        {
            // 删除就的索引文件
            try 
            {
                specIndexZipTempPath = repositoryPathResolver.resolve(storageId, repositoryId, specIndexZipTempUri);
                artifactManagementService.delete(specIndexZipTempPath, true);
                logger.error("删除旧的索引文件成功（{}）", specIndexZipTempPath);
            }
            catch (Exception e)
            { logger.error("删除旧的索引文件失败", e); }
            if(url.contains("github.com")){
                // 下载代理索引zip
                specIndexZipTempPath = artifactResolutionService.resolvePath(storageId, repositoryId, url, specIndexZipTempUri);
                if (!Files.exists(specIndexZipTempPath)) {
                    throw new RuntimeException("下载Cocoapods远程仓库索引Zip失败");
                }
            }else if(url.matches(URL_PATTERN)) {
                specIndexZipTempPath = artifactResolutionService.resolvePath(storageId, repositoryId, url2, specIndexTarGzTempUri);
            }else {
                throw new RuntimeException("Cocoapods 不支持代理库："+url);
            }


            ziFilePath = specIndexZipTempPath.getTarget().toString();
            String tarGzFilePath = specIndexZipTempPath.getTarget().getParent().getParent().toString()+"/master.tar.gz";
            if (S3FileSystemStorageProvider.ALIAS.equals(repository.getStorageProvider()))
            { // 转换网路路径为本地路径
                final String localZipFileTempPath = String.format("%s%s/master.zip", indexTempFolderPath, UUID.randomUUID());
                final String localTarGzFileTempPath = String.format("%s%s/master.tar.gz", indexTempFolderPath, UUID.randomUUID());
                ziFilePath = localZipFileTempPath;
                tarGzFilePath = localTarGzFileTempPath;
                FileUtil.touch(new File(ziFilePath));
                FileUtil.touch(new File(tarGzFilePath));
                try (InputStream inputStream = new BufferedInputStream(Files.newInputStream(specIndexZipTempPath))) {
                    // 将S3网络路径缓存到本地
                    FileUtil.writeFromStream(inputStream, localZipFileTempPath);
                    logger.info("S3存储，转存S3文件到本本地：{}", specIndexZipTempPath);
                }
            }

            logger.info("开始转换Cocoapods仓库代理仓库Zip（{}:{}）", specIndexZipTempPath, ziFilePath);
            final JSONObject podNewSourceObj = new JSONObject();
            CompressUtil.zip2Targz(ziFilePath, tarGzFilePath,
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
                        { logger.info("编码错误PodSpecJson文件：{}", zipEntryName); }

                        return extra;
                    }));
            logger.info("结束转换Cocoapods仓库代理仓库Zip（{}）", specIndexZipTempUri);

            if (S3FileSystemStorageProvider.ALIAS.equals(repository.getStorageProvider()))
            { // 存储模式为S3将转换后的索引TarGz上传到S3
                final RepositoryPath indexTarZipPath = repositoryPathResolver.resolve(storageId, repositoryId, specIndexTarGzTempUri);
                try (InputStream inputStream = Files.newInputStream(Path.of(tarGzFilePath))) {
                    artifactManagementService.store(indexTarZipPath, inputStream);
                }
                logger.info("S3存储，回传本地转换后TarGz文件成功：{}", specIndexTarGzTempUri);
            }

            { // 转换成功后把临时文件删除
                try {
                    artifactManagementService.delete(specIndexZipTempPath.getParent(), true);
                } catch (IOException e) {
                    logger.error("删除临时索引文件失败", e);
                }
            }
        }
        catch (Exception e)
        {
            logger.error("下载Cocoapods远程索引失败", e);
            return false;
        }
        finally
        {
            SYNC_COCOAPODS_PROXY_INDEX_LOCK_MAP.remove(repositorySyncLockKey);
        }
        
        return true;
    }

    @Override
    public boolean getSyncProxyIndexLock(Repository repository) 
    {
        final String repositoryLockKey = String.format("%s:%s", repository.getStorage().getId(), repository.getId());
        return SYNC_COCOAPODS_PROXY_INDEX_LOCK_MAP.getOrDefault(repositoryLockKey, false);
    }
}
