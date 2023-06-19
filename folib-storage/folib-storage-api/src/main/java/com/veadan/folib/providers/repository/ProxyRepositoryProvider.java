package com.veadan.folib.providers.repository;


import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.ImmutableMap;
import com.veadan.folib.artifact.archive.TarGzArchiveListingFunction;
import com.veadan.folib.config.FolibPublicUtils;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.io.RepositoryStreamReadContext;
import com.veadan.folib.io.RepositoryStreamWriteContext;
import com.veadan.folib.providers.io.*;
import com.veadan.folib.providers.repository.event.ProxyRepositoryPathExpiredEvent;
import com.veadan.folib.providers.repository.event.RemoteRepositorySearchEvent;
import com.veadan.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.yaml.snakeyaml.Yaml;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Veadan
 * @author veadan
 */
@Component
public class ProxyRepositoryProvider
        extends AbstractRepositoryProvider
{

    private static final Logger logger = LoggerFactory.getLogger(ProxyRepositoryProvider.class);

    private static final String ALIAS = "proxy";

    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    @Inject
    private HostedRepositoryProvider hostedRepositoryProvider;

    @Inject
    private RepositoryPathLock repositoryPathLock;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @Autowired
    protected ArtifactManagementService artifactManagementService;

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @Override
    protected InputStream getInputStreamInternal(RepositoryPath path)
        throws IOException
    {
        return hostedRepositoryProvider.getInputStreamInternal(path);
    }

    @Override
    protected RepositoryPath fetchPath(RepositoryPath repositoryPath)
        throws IOException
    {
        RepositoryPath targetPath = hostedRepositoryProvider.fetchPath(repositoryPath);
        if (targetPath == null)
        {
            targetPath = resolvePathExclusive(repositoryPath);
        }
        else if (RepositoryFiles.hasExpired(targetPath))
        {
            eventPublisher.publishEvent(new ProxyRepositoryPathExpiredEvent(targetPath));
        }

        return targetPath;
    }

    private RepositoryPath resolvePathExclusive(RepositoryPath repositoryPath)
            throws IOException
    {

        ReadWriteLock lockSource = repositoryPathLock.lock(repositoryPath, "pre-remote-fetch");
        Lock lock = lockSource.writeLock();
        lock.lock();
        try
        {
            return proxyRepositoryArtifactResolver.fetchRemoteResource(repositoryPath);
        }
        catch (IOException e)
        {
            logger.error("Failed to resolve Path for proxied artifact [{}]",
                         repositoryPath, e);

            throw e;
        }
        finally
        {
            lock.unlock();
        }
    }

    @Override
    protected OutputStream getOutputStreamInternal(RepositoryPath repositoryPath)
            throws IOException
    {
        return Files.newOutputStream(repositoryPath);
    }

    @Override
    public List<Path> search(String storageId,
                             String repositoryId,
                             RepositorySearchRequest predicate,
                             Paginator paginator)
    {
        if (Objects.isNull(predicate.getNotPublishEvent()) || Boolean.FALSE.equals(predicate.getNotPublishEvent())) {
            RemoteRepositorySearchEvent event = new RemoteRepositorySearchEvent(storageId,
                    repositoryId,
                    predicate,
                    paginator);
            eventPublisher.publishEvent(event);
        }

        return hostedRepositoryProvider.search(storageId, repositoryId, predicate, paginator);
    }

    @Override
    public Long count(String storageId,
                      String repositoryId,
                      RepositorySearchRequest predicate)
    {
        return hostedRepositoryProvider.count(storageId, repositoryId, predicate);
    }

    @Override
    public Map<String, Object> searchConanPackage(Repository repository, String query) throws Exception {
        String siteUrl = repository.getRemoteRepository().getUrl();
        String pingUrl = siteUrl.endsWith("/") ? siteUrl + "v1/ping" : siteUrl + "/v1/ping";
        String searchPackage = siteUrl.endsWith("/") ? siteUrl + "v1/conans/search" : siteUrl + "/v1/conans/search";

        Client client = clientPool.getRestClient();
        WebTarget pingTarget = client.target(pingUrl);
        Response response = pingTarget.request().get();
        if (response.getStatus() != 200) {
            throw new Exception("{} get error" + pingUrl);
        }
        WebTarget searchTarget = client.target(StringUtils.isNotBlank(query) ? searchPackage + "?q=" + query : searchPackage);
        Response searchResponse = searchTarget.request().get();
        if (searchResponse.getStatus() != 200) {
            throw new Exception("{} get error" + searchPackage);
        }
        return searchResponse.readEntity(Map.class);
    }

    @Override
    public Map<String, Object> searchConanDownLoadUrl(Repository repository, String name, String version, String username, String channel) {
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        String remoteRepositoryUrl = repository.getRemoteRepository().getUrl();

        Client client = clientPool.getRestClient();
        WebTarget target = client.target(String.format("%s/v1/conans/%s/%s/%s/%s/download_urls", remoteRepositoryUrl, name, version, username, channel));
        Response response = target.request(MediaType.APPLICATION_JSON_TYPE).get();
        String jsonResult = response.readEntity(String.class);
        JSONObject jsonObj = JSONObject.parseObject(jsonResult);
        List<String> list = Arrays.asList("conan_export.tgz", "conanmanifest.txt", "conanfile.py");

        String filePathTemplate = String.format("%s/%s/%s/%s/export", name, version, username, channel);

        for (String filename : list) {
            try {
                String remoteUrl = jsonObj.getString(filename);
                Response res = client.target(remoteUrl).request().get();
                if (res.getStatus() != 200) {
                    logger.error("{} get error", remoteUrl);
                    continue;
                }
                InputStream is = res.readEntity(InputStream.class);
                String filePath = filePathTemplate + "/" + filename;
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, filePath);
                logger.info("conan {} {} upload path {}", storageId, repositoryId, filePath);
                artifactManagementService.store(repositoryPath, is);

                if ("conan_export.tgz".equals(filename)) {
                    byte[] contentByFileName = TarGzArchiveListingFunction.INSTANCE.getContentByFileName(repositoryPath, "conandata.yml");
                    Map<String, Map<String, Map<String, Object>>> properties;
                    Yaml yaml = new Yaml();
                    properties = yaml.load(new ByteArrayInputStream(contentByFileName));
                    Object o = properties.get("sources").get(version).get("url");
                    String targetSourceUrl;
                    if (o instanceof String) {
                        targetSourceUrl = (String) o;
                    } else {
                        targetSourceUrl = (String) ((List<?>) o).get(0);
                    }
                    String targetSourceFilename = targetSourceUrl.substring(targetSourceUrl.lastIndexOf("/") + 1);
                    logger.info("conan download target url src package: {}", targetSourceUrl);
                    Response targetSourceRes = client.target(targetSourceUrl).request().get();
                    InputStream targetSourceIs = targetSourceRes.readEntity(InputStream.class);
                    String targetSourcePath = String.format("%s/%s/%s/%s/package/%s", name, version, username, channel, targetSourceFilename);
                    RepositoryPath targetSourceRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, targetSourcePath);
                    logger.info("conan {} {} upload path {}", storageId, repositoryId, filePath);
                    artifactManagementService.store(targetSourceRepositoryPath, targetSourceIs);
                }
            } catch (IOException e) {
                logger.error("conan {} {} download or store target file error.", storageId, repositoryId, e);
            }
        }

        String localUrl = FolibPublicUtils.getRepositoryWebServerUrl(repository);
        String localConanExportTgz = String.format("%s/v1/files/%s/conan_export.tgz", localUrl, filePathTemplate);
        String localConanManifestTxt = String.format("%s/v1/files/%s/conanmanifest.txt", localUrl, filePathTemplate);
        String localConanFilePy = String.format("%s/v1/files/%s/conanfile.py", localUrl, filePathTemplate);
        return ImmutableMap.<String, Object>builder()
                .put("conan_export.tgz", localConanExportTgz)
                .put("conanmanifest.txt", localConanManifestTxt)
                .put("conanfile.py", localConanFilePy)
                .build();
    }

    @Override
    public Map<String, Object> searchConanPackageInfo(Repository repository, String packageName, String version) {
        String url = repository.getRemoteRepository().getUrl();
        String reqUrl = url.endsWith("/") ? url + "v1/conans/" + packageName + "/" + version + "/_/_/search" :
                url + "/v1/conans/" + packageName + "/" + version + "/_/_/search";
        Client client = clientPool.getRestClient();
        WebTarget target = client.target(reqUrl);
        Response response = target.request().get();
        if (response.getStatus() != 200) {
            logger.error("requset {} error", reqUrl);
            return null;
        }
        return response.readEntity(Map.class);
    }

    @Override
    protected Artifact provideArtifact(RepositoryPath repositoryPath) throws IOException
    {
        Artifact artifactEntry = super.provideArtifact(repositoryPath);
        if (artifactEntry.getNativeId() == null) {
            artifactEntry = new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                                      RepositoryFiles.readCoordinates(repositoryPath));
            artifactEntry.setArtifactFileExists(Boolean.FALSE);
        }
        
        return artifactEntry;
    }

    @Override
    protected boolean shouldStoreArtifact(Artifact artifactEntry)
    {
        boolean result = super.shouldStoreArtifact(artifactEntry) || !artifactEntry.getArtifactFileExists();
        artifactEntry.setArtifactFileExists(true);
        
        return result;
    }

    @Override
    public void commit(RepositoryStreamWriteContext ctx)
        throws IOException
    {
        super.commit(ctx);
    }

    @Override
    public void commitStoreIndex(RepositoryStreamReadContext ctx)
            throws IOException
    {
        super.commitStoreIndex(ctx);
    }

    @Override
    public void onStoreIndexAfter(RepositoryStreamReadContext ctx)
            throws IOException
    {
        super.onStoreIndexAfter(ctx);
    }

}
