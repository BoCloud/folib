package com.veadan.folib.services.impl;

import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.Component;
import com.veadan.folib.forms.component.ArtifactGraphForm;
import com.veadan.folib.forms.component.ArtifactStatisticsForm;
import com.veadan.folib.forms.component.ComponentTableForm;
import com.veadan.folib.forms.vulnerability.AffectedArtifactsForm;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.repositories.ComponentRepository;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ComponentService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * @date 2023/5/24
 **/
@Slf4j
@Service
public class ComponentServiceImpl implements ComponentService {

    @Autowired
    private ComponentRepository componentRepository;

    @Autowired
    private ArtifactRepository artifactRepository;

    @Autowired
    private ConfigurationManager configurationManager;

    @Autowired
    @Lazy
    private ArtifactComponent artifactComponent;

    @Override
    public TableResultResponse<ComponentTableForm> queryComponentPage(Integer page, Integer limit, String name, String groupId, String version, String searchKeyword) {
        Pageable pageable = null;
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        if (page == 1) {
            pageable = PageRequest.of(page, limit).first();
        } else {
            pageable = PageRequest.of(page, limit).previous();
        }
        Page<Component> componentPage = componentRepository.queryComponentPage(pageable, name, groupId, version, searchKeyword);
        return new TableResultResponse<ComponentTableForm>(componentPage.getTotalElements(), transform(componentPage));
    }

    @Override
    public TableResultResponse<ComponentTableForm> queryComponentPageByArtifact(Integer page, Integer limit, String artifactPath, String searchKeyword) {
        Pageable pageable = null;
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        if (page == 1) {
            pageable = PageRequest.of(page, limit).first();
        } else {
            pageable = PageRequest.of(page, limit).previous();
        }
        Page<Component> componentPage = componentRepository.queryComponentPageByArtifact(pageable, artifactPath, searchKeyword);
        return new TableResultResponse<ComponentTableForm>(componentPage.getTotalElements(), transform(componentPage));
    }

    @Override
    public ComponentTableForm queryComponentOne(String uuid) {
        Optional<Component> optionalComponent = componentRepository.findById(uuid);
        ComponentTableForm componentTableForm = null;
        if (optionalComponent.isPresent()) {
            componentTableForm = ComponentTableForm.builder().build();
            BeanUtils.copyProperties(optionalComponent.get(), componentTableForm);
        }
        return componentTableForm;
    }

    @Override
    public TableResultResponse<AffectedArtifactsForm> queryArtifactByComponentUuid(Integer page, Integer limit, String componentUuid, String searchKeyword) {
        Pageable pageable = null;
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        if (page == 1) {
            pageable = PageRequest.of(page, limit).first();
        } else {
            pageable = PageRequest.of(page, limit).previous();
        }
        org.springframework.data.domain.Page<Artifact> artifactPage = artifactRepository.queryArtifactByComponentUuid(pageable, componentUuid, searchKeyword);
        List<AffectedArtifactsForm> affectedArtifacts = Collections.emptyList();
        if (Objects.nonNull(artifactPage) && CollectionUtils.isNotEmpty(artifactPage.getContent())) {
            affectedArtifacts = artifactPage.getContent().stream().map(artifact -> {
                AffectedArtifactsForm affectedArtifactsForm = AffectedArtifactsForm.builder().uuid(artifact.getUuid()).storageId(artifact.getStorageId()).repositoryId(artifact.getRepositoryId()).build();
                Repository repository = configurationManager.getRepository(artifact.getStorageId(), artifact.getRepositoryId());
                affectedArtifactsForm.setLayout(repository.getLayout());
                String path = artifact.getArtifactCoordinates().buildPath();
                if (DockerLayoutProvider.ALIAS.equalsIgnoreCase(repository.getLayout())) {
                    //docker
                    affectedArtifactsForm.setArtifactName(path.substring(path.indexOf("/") + 1, path.indexOf("/sha256")));
                    affectedArtifactsForm.setArtifactPath(path.substring(0, path.indexOf("/sha256")));
                } else {
                    affectedArtifactsForm.setArtifactName(path.substring(path.lastIndexOf("/") + 1));
                    affectedArtifactsForm.setArtifactPath(path);
                }
                return affectedArtifactsForm;
            }).collect(Collectors.toList());
        }
        return new TableResultResponse<AffectedArtifactsForm>(artifactPage.getTotalElements(), affectedArtifacts);
    }

    @Override
    public ArtifactGraphForm artifactGraph(String componentUuid) {
        ArtifactGraphForm artifactGraphForm = null;
        Optional<Component> optionalComponent = componentRepository.findById(componentUuid);
        if (optionalComponent.isPresent()) {
            List<Artifact> artifactList = artifactRepository.queryArtifactByComponentUuid(componentUuid);
            if (CollectionUtils.isNotEmpty(artifactList)) {
                artifactGraphForm = ArtifactGraphForm.builder().build();
                artifactGraphForm.setId(componentUuid);
                artifactGraphForm.setTitle("组件信息");
                artifactGraphForm.setName(optionalComponent.get().getName());
                artifactGraphForm.setVersion(optionalComponent.get().getVersion());

                List<ArtifactGraphForm> storageGraphList = Lists.newArrayList();
                ArtifactGraphForm storageGraph, artifactGraph;
                Map<String, List<Artifact>> repositoryIdMap;
                List<ArtifactGraphForm> repositoryGraphList, artifactGraphList;
                String artifactName;
                ArtifactGraphForm repositoryGraph;
                Repository repository;
                boolean isDockerLayout;
                //存储空间分组
                Map<String, List<Artifact>> storageIdMap = artifactList.stream().collect(Collectors.groupingBy(Artifact::getStorageId, Collectors.toCollection(LinkedList::new)));
                for (Map.Entry<String, List<Artifact>> storageEntry : storageIdMap.entrySet()) {
                    storageGraph = ArtifactGraphForm.builder().id(storageEntry.getKey()).title("所属空间").name(storageEntry.getKey()).build();
                    //仓库分组
                    repositoryIdMap = storageEntry.getValue().stream().collect(Collectors.groupingBy(Artifact::getRepositoryId, Collectors.toCollection(LinkedList::new)));
                    repositoryGraphList = Lists.newArrayList();
                    for (Map.Entry<String, List<Artifact>> repositoryEntry : repositoryIdMap.entrySet()) {
                        repository = configurationManager.getRepository(storageEntry.getKey(), repositoryEntry.getKey());
                        isDockerLayout = Objects.nonNull(repository) && DockerLayoutProvider.ALIAS.equalsIgnoreCase(repository.getLayout());
                        repositoryGraph = ArtifactGraphForm.builder().id(String.format("%s-%s", storageEntry.getKey(), repositoryEntry.getKey())).title("所属仓库").name(repositoryEntry.getKey()).build();
                        artifactGraphList = Lists.newArrayList();
                        for (Artifact artifact : repositoryEntry.getValue()) {
                            artifactName = artifact.getArtifactPath();
                            if (isDockerLayout) {
                                artifactName = artifactComponent.getDockerImage(artifact.getArtifactPath());
                            }
                            artifactGraph = ArtifactGraphForm.builder().id(artifact.getUuid()).title("制品信息").name(artifactName).count(artifact.getDownloadCount()).up(false)
                                    .count(artifact.getDownloadCount()).color("B").build();
                            artifactGraphList.add(artifactGraph);
                        }
                        repositoryGraph.setChildren(artifactGraphList);
                        repositoryGraphList.add(repositoryGraph);
                    }
                    storageGraph.setChildren(repositoryGraphList);
                    storageGraphList.add(storageGraph);
                }
                artifactGraphForm.setChildren(storageGraphList);
            }
        }
        return artifactGraphForm;
    }

    @Override
    public ArtifactStatisticsForm artifactStatistics(String componentUuid) {
        int zero = 0;
        ArtifactStatisticsForm artifactStatisticsForm = ArtifactStatisticsForm.builder().storageCount(zero).repositoryCount(zero).artifactCount(zero).build();
        List<Artifact> artifactList = artifactRepository.queryArtifactByComponentUuid(componentUuid);
        if (CollectionUtils.isNotEmpty(artifactList)) {
            artifactStatisticsForm.setStorageCount((int) artifactList.stream().map(Artifact::getStorageId).distinct().count());
            artifactStatisticsForm.setRepositoryCount((int) artifactList.stream().map(Artifact::getRepositoryId).distinct().count());
            artifactStatisticsForm.setArtifactCount(artifactList.size());
        }
        return artifactStatisticsForm;
    }

    private List<ComponentTableForm> transform(Page<Component> componentPage) {
        List<ComponentTableForm> list = componentPage.getContent().stream().map(component -> {
            ComponentTableForm componentTableForm = ComponentTableForm.builder().build();
            BeanUtils.copyProperties(component, componentTableForm);
            return componentTableForm;
        }).collect(Collectors.toList());
        return list;
    }


}
