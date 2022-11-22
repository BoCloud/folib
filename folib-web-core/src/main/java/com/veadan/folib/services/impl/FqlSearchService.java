package com.veadan.folib.services.impl;

import cn.hutool.core.date.DateUtil;
import com.veadan.folib.data.criteria.Selector;
import com.veadan.folib.dependency.snippet.CodeSnippet;
import com.veadan.folib.dependency.snippet.SnippetGenerator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.AqlSearchService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.search.SearchResult;
import com.veadan.folib.storage.search.SearchResults;
import com.veadan.folib.utils.TreeUtil;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.io.IOException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.Set;

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

    @Override
    public SearchResults search(Selector<ArtifactEntity> selector) throws IOException {

        return null;
    }

    @Override
    protected EntityTraversalAdapter<Vertex, Artifact> adapter() {
        return artifactAdapter;
    }


    public SearchResults artfactQuery(String artifactName,
                                      String storageId,
                                      String repositoryId,
                                      String beginDate,
                                      String endDate,
                                      String sortField,
                                      String sortOrder,
                                      int limit, int page) throws IOException {

        Pageable pageable = null;
        if (page == 1) {
            pageable = PageRequest.of(page, limit).first();
        } else {
            pageable = PageRequest.of(page, limit).previous();
        }
        Page<Artifact> artifacts = artifactRepository.findMatchingByIndex(pageable, artifactName, storageId, repositoryId, beginDate, endDate, sortField, sortOrder);
        List<Artifact> artifactEntityList = artifacts.getContent();

        SearchResults result = new SearchResults();
        result.setTotal(artifacts.getTotalElements());
        SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        for (Artifact artifact : artifactEntityList) {
            SearchResult r = new SearchResult();
            result.getResults().add(r);

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
            String lastUpdatedTime = DateUtil.format(Date.from(artifact.getLastUsed().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
            String lastUsedTime = DateUtil.format(Date.from(artifact.getLastUsed().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
            r.setLastUpdated(lastUpdatedTime);
            r.setLastUsed(lastUsedTime);
            r.setSha(artifact.getChecksums().get("SHA-1"));
            r.setMd5(artifact.getChecksums().get("MD5"));

            //生成snippets
            Repository repository = repositoryPath.getRepository();
            URL artifactResource = RepositoryFiles.readResourceUrl(repositoryPath);
            r.setUrl(artifactResource.toString());

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
}
