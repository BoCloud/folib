package com.veadan.folib.services.impl;

import cn.hutool.core.date.DateUtil;
import com.alibaba.fastjson.JSON;
import com.google.common.collect.Lists;
import com.veadan.folib.config.FolibPublicUtils;
import com.veadan.folib.data.criteria.Selector;
import com.veadan.folib.dependency.snippet.CodeSnippet;
import com.veadan.folib.dependency.snippet.SnippetGenerator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.domain.FileContent;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.services.AqlSearchService;
import com.veadan.folib.services.DirectoryListingService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.search.SearchResult;
import com.veadan.folib.storage.search.SearchResults;
import com.veadan.folib.util.RepositoryPathUtil;
import com.veadan.folib.utils.TreeUtil;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.text.SimpleDateFormat;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

@Component
@Transactional
public class FqlSearchService extends GremlinVertexRepository<Artifact> implements AqlSearchService {
    @Inject
    ArtifactAdapter artifactAdapter;

    @Inject
    ArtifactRepository artifactRepository;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;
    @Inject
    private SnippetGenerator snippetGenerator;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    @Override
    public SearchResults search(Selector<ArtifactEntity> selector) throws IOException {

        return null;
    }

    @Override
    protected EntityTraversalAdapter<Vertex, Artifact> adapter() {
        return artifactAdapter;
    }


    public SearchResults artifactQuery(Boolean regex, String artifactName,
                                       String metadataSearch,
                                       String storageId,
                                       String repositoryId,
                                       String beginDate,
                                       String endDate,
                                       String sortField,
                                       String sortOrder,
                                       Integer limit, Integer page) throws IOException {

        Pageable pageable = null;
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 5;
        }
        if (page == 1) {
            pageable = PageRequest.of(page, limit).first();
        } else {
            pageable = PageRequest.of(page, limit).previous();
        }
        Page<Artifact> artifacts = artifactRepository.findMatchingByIndex(pageable, regex, artifactName, metadataSearch, storageId, repositoryId, beginDate, endDate, sortField, sortOrder);
        List<Artifact> artifactEntityList = artifacts.getContent();

        SearchResults result = new SearchResults();
        result.setTotal(artifacts.getTotalElements());
        SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        for (Artifact artifact : artifactEntityList) {
            SearchResult r = new SearchResult();
            result.getResults().add(r);

            r.setMetadata(artifact.getMetadata());
            r.setStorageId(artifact.getStorageId());
            r.setRepositoryId(artifact.getRepositoryId());
            r.setArtifactCoordinates(artifact.getArtifactCoordinates());

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(),
                    artifact.getRepositoryId(),
                    artifact.getArtifactPath());

            r.setChecksums(artifact.getChecksums());
            r.setSizeInBytes(artifact.getSizeInBytes());
            r.setDownloadCount(artifact.getDownloadCount());

            String createdTime = DateUtil.format(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
            r.setCreated(createdTime);
            if(Objects.nonNull(artifact.getLastUsed())){
                String lastUpdatedTime = DateUtil.format(Date.from(artifact.getLastUsed().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                String lastUsedTime = DateUtil.format(Date.from(artifact.getLastUsed().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                r.setLastUpdated(lastUpdatedTime);
                r.setLastUsed(lastUsedTime);
            }
            r.setSha(artifact.getChecksums().get("SHA-1"));
            r.setMd5(artifact.getChecksums().get("MD5"));

            //生成snippets
            Repository repository = repositoryPath.getRepository();
            URL artifactResource = RepositoryFiles.readResourceUrl(repositoryPath);
            r.setUrl(artifactResource.toString());
            r.setLayout(repository.getLayout());
            String path = artifact.getArtifactCoordinates().buildPath();
            if ("Docker".equalsIgnoreCase(r.getLayout())) {
                //docker
                r.setArtifactName(path.substring(path.indexOf("/") + 1, path.indexOf("/sha256")));
                r.setArtifactPath(path.substring(0, path.indexOf("/sha256")));
                String blobs = "blobs";
                String manifest = "manifest";
                String artifactPath = repositoryPath.toAbsolutePath().toString();
                if (artifactPath.contains("sha256") && !artifactPath.contains(blobs) && !artifactPath.contains(manifest) && !artifactPath.endsWith(".sha256")) {
                    r.setSizeInBytes(getSearchDockerSize(storageId, repositoryId, repositoryPath, path));
                }
                r.setDownloadFilesUrl(getDockerDownLoadAppPackageUrls(storageId, repositoryId, repositoryPath,
                        path.substring(0, path.indexOf("/sha256")) + "/temp"));
            } else {
                r.setArtifactName(path.substring(path.lastIndexOf("/") + 1));
                r.setArtifactPath(path);
            }
            List<CodeSnippet> snippets = snippetGenerator.generateSnippets(repository.getLayout(),
                    artifact.getArtifactCoordinates());
            r.setSnippets(snippets);

            TreeUtil treeUtil = new TreeUtil();
            Set<String> fileNames = artifact.getArtifactArchiveListing().getFilenames();
            if (fileNames != null && fileNames.size() > 0) {
                List listTree = treeUtil.toTree(fileNames);
                r.setTreeNode(listTree);
            }
        }
        return result;
    }

    private Set<String> getDockerDownLoadAppPackageUrls(String storageId, String repositoryId, RepositoryPath repositoryPath, String path) {
        Set<String> downloadUrls = new HashSet<String>();
        try {
            RepositoryPath appPackagePath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            List<String> relativePaths = RepositoryPathUtil.getFileRelativePaths(appPackagePath);
            for (String filePath : relativePaths) {
                if (filePath.endsWith(".config") || filePath.endsWith(".sha256")) {
                    continue;
                }
                downloadUrls.add(FolibPublicUtils.getFileUrl(repositoryPath.getRepository(), filePath));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return downloadUrls;
    }

    private Long getSearchDockerSize(String storageId, String repositoryId, RepositoryPath repositoryPath, String path) throws IOException {
        String versionPath = path.substring(0, path.indexOf("/sha256"));
        String imagesName = versionPath.split("/")[0];
        String blobsPath = imagesName + "/blobs";
        RepositoryPath repositoryPathBlobs = repositoryPathResolver.resolve(storageId, repositoryId, blobsPath);

        //获取blobs下的文件列表，为了获取层的大小。
        DirectoryListing blobsListing = directoryListingService.fromRepositoryPath(repositoryPathBlobs);
        String menifestString = Files.readString(repositoryPath);
        ImageManifest menifest = JSON.parseObject(menifestString, ImageManifest.class);
        List<String> digestList = menifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
        List<FileContent> fileblobs = Optional.ofNullable(blobsListing.getFiles()).orElse(Lists.newArrayList()).stream().filter(file -> digestList.contains(file.getName())).collect(Collectors.toList());
        return fileblobs.stream().mapToLong(FileContent::getSize).sum();
    }
}
