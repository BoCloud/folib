package com.veadan.folib.providers.repository;


import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;

import com.veadan.folib.config.FolibPublicUtils;
import com.veadan.folib.providers.io.*;
import com.veadan.folib.providers.repository.event.ProxyRepositoryPathExpiredEvent;
import com.veadan.folib.providers.repository.event.RemoteRepositorySearchEvent;
import com.veadan.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.io.RepositoryStreamWriteContext;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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
        RemoteRepositorySearchEvent event = new RemoteRepositorySearchEvent(storageId,
                                                                            repositoryId,
                                                                            predicate,
                                                                            paginator);
        eventPublisher.publishEvent(event);

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
    public Map<String, Object> searchConanDownLoadUrl(Repository repository, String packageName, String version) {
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        String url = repository.getRemoteRepository().getUrl();
        Map<String, Object> map = new HashMap<String, Object>();
        String conanExportTgz = url + "/v1/files/_/" + packageName + "/" + version + "/_/0/export/conan_export.tgz";
        String conanManifestTxt = url + "/v1/files/_/" + packageName + "/" + version + "/_/0/export/conanmanifest.txt";
        String conanFilePy = url + "/v1/files/_/" + packageName + "/" + version + "/_/0/export/conanfile.py";

        String localUrl = FolibPublicUtils.getRepositoryWebServerUrl(repository);
        String localConanExportTgz = localUrl + "/v1/files/_/" + packageName + "/" + version + "/_/0/export/conan_export.tgz";
        String localConanManifestTxt = localUrl + "/v1/files/_/" + packageName + "/" + version + "/_/0/export/conanmanifest.txt";
        String localConanFilePy = localUrl + "/v1/files/_/" + packageName + "/" + version + "/_/0/export/conanfile.py";
        map.put("conan_export.tgz", conanExportTgz);
        map.put("conanmanifest.txt", conanManifestTxt);
        map.put("conanfile.py", conanFilePy);
        // 拉取文件到本地
        Client client = clientPool.getRestClient();
        List<String> list = Arrays.asList("conan_export.tgz", "conanmanifest.txt", "conanfile.py");
        for (String file : list) {
            try {
                WebTarget target = client.target(map.get(file).toString());
                Response response = target.request().get();
                if (response.getStatus() != 200) {
                    logger.error("{} get error", map.get(file).toString());
                    continue;
                }
                InputStream is = response.readEntity(InputStream.class);
                String filePath = "_/" + packageName + "/" + version + "/_/0/export/" + file;

                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId,
                        repositoryId, filePath);
                logger.info("conan {} {} upload path {}", storageId, repositoryId, filePath);
                artifactManagementService.store(repositoryPath, is);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        map.put("conan_export.tgz", localConanExportTgz);
        map.put("conanmanifest.txt", localConanManifestTxt);
        map.put("conanfile.py", localConanFilePy);
        return map;
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
    
}
