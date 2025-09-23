package com.folib.index.indexer;

import com.folib.domain.Artifact;
import com.folib.index.model.*;
import com.folib.index.utils.CondaUtils;
import com.folib.index.utils.CondaVersionComparator;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;


@Component
public class CondaMetadataIndexer {
    private final CondaMetadataExtractor extractor;

    private final RepositoryPathResolver repositoryPathResolver;

    @Autowired
    public CondaMetadataIndexer(CondaMetadataExtractor extractor, RepositoryPathResolver repositoryPathResolver) {
        this.extractor = extractor;
        this.repositoryPathResolver = repositoryPathResolver;
    }

//    @VisibleForTesting
//    void setCondaMetadataExtractor(CondaMetadataExtractor extractor) {
//        this.extractor = extractor;
//    }


    /**
     * @param repoKey
     * @param artifactName
     * @Description: 将包转化为包索引数据
     */
    @Nullable
    public RepoDataPackage getRepoDataPackage(@NonNull String repoKey, @NonNull String artifactName) {
        if (repoKey == null || artifactName == null) {
            return null;
        } else {
            Index index = this.extractor.extract(repoKey, artifactName);
            if (index == null) {
                return null;
            } else {
                RepoDataPackage repoDataPackage = IndexToRepodataPackageAdapter.adapt(index);
                try {
                    this.supplementRepoDataPackage(repoDataPackage, repoKey, artifactName);
                } catch (IOException e) {
                    return null;
                }
                return repoDataPackage;
            }
        }
    }

    /**
     * @param repoKey
     * @param artifactName
     * @Description: 将包转化为包索引数据
     */
    @Nullable
    public RepoDataPackage getRepoDataPackageWithIndex(@NonNull String repoKey, @NonNull String artifactName, @NonNull Index index) {
        if (repoKey == null || artifactName == null) {
            return null;
        } else {
            if (index == null) {
                return null;
            } else {
                RepoDataPackage repoDataPackage = IndexToRepodataPackageAdapter.adapt(index);
                try {
                    this.supplementRepoDataPackage(repoDataPackage, repoKey, artifactName);
                } catch (IOException e) {
                    return null;
                }
                return repoDataPackage;
            }
        }
    }


    /**
     * @param platformId
     * @Description: 创建新的索引数据
     */
    public RepoData createNewRepoData(@NonNull String platformId) {
        RepoData repoData = new RepoData();
        repoData.setInfo(new RepoDataInfo());
        repoData.getInfo().setSubdir(platformId);
        return repoData;
    }


    /**
     * @param repoData
     * @param repoDataPackage
     * @param artifactName
     * @Description: 添加包到索引数据中
     */
    public void addPackageToRepodata(RepoData repoData,
                                     RepoDataPackage repoDataPackage,
                                     String artifactName) {
        if (repoDataPackage == null) {
            return;
        }
        SortedMap<String, RepoDataPackage> packageSortedMap = CondaUtils.isTarBz2File(artifactName) ? repoData.getPackages() : repoData.getCondaPackages();
        packageSortedMap.put(artifactName, repoDataPackage);
    }


    /**
     * @param currentRepoData
     * @param repoData
     * @param repoDataPackage
     * @param artifactName
     * @Description: 添加包到当前的索引数据中, 先删除包, 再从repoData中获取最新的包并添加
     */
    public void addPackageToCurrentRepoData(RepoData currentRepoData,
                                            RepoData repoData,
                                            RepoDataPackage repoDataPackage,
                                            String artifactName) {
        if (repoDataPackage == null) {
            return;
        }
        SortedMap<String, RepoDataPackage> packageSortedMap = CondaUtils.isTarBz2File(artifactName) ? repoData.getPackages() : repoData.getCondaPackages();
        SortedMap<String, RepoDataPackage> packageSortedCurrentMap = CondaUtils.isTarBz2File(artifactName) ? currentRepoData.getPackages() : currentRepoData.getCondaPackages();
        String repoDataPackageName = repoDataPackage.getName();
        Map<String, RepoDataPackage> currentPackagesMap = CondaUtils.findMapByName(packageSortedCurrentMap, repoDataPackageName);
        Map<String, RepoDataPackage> allPackagesMap = CondaUtils.findMapByName(packageSortedMap, repoDataPackageName);
        Map<String, RepoDataPackage> allLatestOfArtifact = this.getAllLatestOfArtifact(repoDataPackageName, allPackagesMap);
        packageSortedCurrentMap.keySet().removeAll(currentPackagesMap.keySet());
        packageSortedCurrentMap.putAll(allLatestOfArtifact);
    }


    /**
     * @param repoData
     * @param artifactName
     * @Description: 删除包
     */
    public void removePackageFromRepodata(RepoData repoData,
                                          String artifactName) {
        if (checkPackageExistsInRepoData(repoData, artifactName)) {
            SortedMap<String, RepoDataPackage> packageSortedMap = CondaUtils.isTarBz2File(artifactName) ? repoData.getPackages() : repoData.getCondaPackages();
            packageSortedMap.remove(artifactName);
        }
    }


    /**
     * @param repoDataList
     * @Description: 聚合同平台所有索引数据
     */
    public RepoData aggregateRepoData(List<RepoData> repoDataList)
            throws Exception {
        // 1. 过滤掉空数据
        List<RepoData> filteredRepoDataList = repoDataList.stream()
                .filter(repoData -> repoData != null && repoData.getPackages() != null)
                .collect(Collectors.toList());
        // 2. 判断所有RepoData是否都属于同一平台
        if (filteredRepoDataList.isEmpty()) {
            return null;
        }
        String platformId = filteredRepoDataList.get(0).getInfo().getSubdir();
        for (RepoData repoData : filteredRepoDataList) {
            if (!StringUtils.equals(repoData.getInfo().getSubdir(), platformId)) {
                throw new Exception("All RepoData must belong to the same platform");
            }
        }
        // 3. 创建新的索引数据
        RepoData aggregatedRepoData = this.createNewRepoData(platformId);
        // 4. 聚合索引数据
        for (RepoData repoData : filteredRepoDataList) {
            if (repoData.getPackages() != null) {
                aggregatedRepoData.getPackages().putAll(repoData.getPackages());
            }
            if (repoData.getCondaPackages() != null) {
                aggregatedRepoData.getCondaPackages().putAll(repoData.getCondaPackages());
            }
        }
        // 5. 返回聚合后的索引数据
        return aggregatedRepoData;
    }


    public boolean checkPackageExistsInRepoData(RepoData repoData,
                                                String artifactName) {
        SortedMap<String, RepoDataPackage> packageSortedMap = CondaUtils.isTarBz2File(artifactName) ? repoData.getPackages() : repoData.getCondaPackages();
        return packageSortedMap.containsKey(artifactName);
    }

    private void supplementRepoDataPackage(RepoDataPackage repoDataPackage, String repoKey,
                                           @NonNull String artifactName) throws IOException {
        // 1. 分解repoKey后三位 .../{storageId}/{repositoryId}/{platform}
        String[] repoKeyParts = repoKey.split("/");
        if (repoKeyParts.length < 3) {
            return;
        }
        String storageId = repoKeyParts[repoKeyParts.length - 3];
        String repositoryId = repoKeyParts[repoKeyParts.length - 2];
        String platform = repoKeyParts[repoKeyParts.length - 1];
        // 2. 获取存储路径
        RepositoryPath repoPath = repositoryPathResolver.resolve(storageId, repositoryId, platform + "/" + artifactName);
        // 3. 获取文件实体
        Artifact artifact = repoPath.getArtifactEntry();


        repoDataPackage.setSize(artifact.getSizeInBytes());
        if (repoDataPackage.getNoarch() == null) {
            this.findNoArchInMetaYaml(repoKey, repoDataPackage, artifactName);
        }
    }


    private void findNoArchInMetaYaml(String repoKey, RepoDataPackage repoDataPackage, String artifactName) {
        MetaYaml metaYaml = (MetaYaml) this.extractor.getIndex(repoKey, artifactName, "meta.yaml", MetaYaml.class,
                CondaMetadataExtractor.MetadataFormat.YAML);
        if (metaYaml != null && metaYaml.getBuild() != null) {
            Object noarch = metaYaml.getBuild().get("noarch");
            if (isEmptyNullOrFalse(noarch)) {
            } else {
                repoDataPackage.setNoarch(noarch);
            }
        } else {
        }
    }


    private static boolean isEmptyNullOrFalse(Object noarch) {
        if (noarch == null) {
            return true;
        }

        if (noarch instanceof Boolean) {
            return !(Boolean) noarch;
        }

        return "''".equals(noarch);
    }


    /**
     * @param artifactName
     * @param artifactsRepoDataPackagesMap
     * @return
     * @Description: 获取最新版本号的所有包
     */
    @Nonnull
    Map<String, RepoDataPackage> getAllLatestOfArtifact(String artifactName, Map<String, RepoDataPackage> artifactsRepoDataPackagesMap) {
        // 1. 获取最新版本号
        Map.Entry<String, RepoDataPackage> latest = this.getMapOfLatest(artifactsRepoDataPackagesMap);
        // 2. 获取并返回最新版本号的所有包
        if (latest == null) {
            return Map.of();
        } else {
            return this.collectLatest(artifactsRepoDataPackagesMap, latest);
        }
    }


    /**
     * @param artifactsRepoDataPackagesMap
     * @return
     * @Description: 获取最新版本号的包(多个则选其一), 主要目的是为了获取最新版本号
     */
    @Nullable
    private Map.Entry<String, RepoDataPackage> getMapOfLatest(Map<String, RepoDataPackage> artifactsRepoDataPackagesMap) {
        CondaVersionComparator condaVersionComparator = CondaVersionComparator.get();
        Optional<Map.Entry<String, RepoDataPackage>> latest = artifactsRepoDataPackagesMap.entrySet().stream()
                .filter(entry -> entry.getValue() != null)
                .max(this.getComparator(condaVersionComparator));
        return latest.orElse(null);
    }


    /**
     * @param artifactsRepoDataPackagesMap
     * @param latest
     * @return
     * @Description: 获取最新版本号的所有包
     */
    @Nonnull
    private Map<String, RepoDataPackage> collectLatest(Map<String, RepoDataPackage> artifactsRepoDataPackagesMap, Map.Entry<String, RepoDataPackage> latest) {
        CondaVersionComparator condaVersionComparator = CondaVersionComparator.get();
        return artifactsRepoDataPackagesMap.entrySet().stream()
                .filter(entry -> entry.getValue() != null)
                .filter(entry -> StringUtils.equals(entry.getValue().getVersion(), latest.getValue().getVersion()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }


    /**
     * @param condaVersionComparator
     * @return
     * @Description: 获取比较器
     */
    @Nonnull
    private Comparator<Map.Entry<String, RepoDataPackage>> getComparator(CondaVersionComparator condaVersionComparator) {
        return (e1, e2) -> condaVersionComparator.compare(((RepoDataPackage) e1.getValue()).getCondaVersion(), ((RepoDataPackage) e2.getValue()).getCondaVersion());
    }


    /**
     * @param repoData
     * @param platformId
     * @return
     * @Description: 重新索引当前的索引数据
     */
    @Nullable
    public RepoData reindexCurrentRepoData(RepoData repoData, String platformId) {
        if (repoData == null) {
            return null;
        } else {
            RepoData currentRepoData = this.createNewRepoData(platformId);
            currentRepoData.setInfo(repoData.getInfo());
            SortedMap<String, RepoDataPackage> tgzRepoDataPackageMap = this.aggregateCurrent(repoData.getPackages());
            currentRepoData.setPackages(tgzRepoDataPackageMap);
            SortedMap<String, RepoDataPackage> condaRepoDataPackageMap = this.aggregateCurrent(repoData.getCondaPackages());
            currentRepoData.setCondaPackages(condaRepoDataPackageMap);
            return currentRepoData;
        }
    }


    /**
     * @param repoKey
     * @return
     * @Description: 扫描整个平台, 重新生成索引repoData.json
     */
    public RepoData reindexRepoData(String repoKey, String platformId) {
        // 1. 创建新的索引数据
        RepoData repoData = this.createNewRepoData(platformId);

        try {
            // 2. 获取所有的conda和tar.bz2文件
            List<Path> condaAndTarBz2Files = this.findCondaAndTarBz2Files(repoKey);

            // 3. 遍历所有的conda和tar.bz2文件, 获取索引数据
            for (Path condaAndTarBz2File : condaAndTarBz2Files) {
                String artifactName = condaAndTarBz2File.getFileName().toString();
                String repoKeyPath = condaAndTarBz2File.getParent().toString();
                RepoDataPackage repoDataPackage = this.getRepoDataPackage(repoKeyPath, artifactName);
                if (repoDataPackage != null) {
                    this.addPackageToRepodata(repoData, repoDataPackage, artifactName);
                }
            }
        } catch (IOException e) {
            throw new RuntimeException("Failed to reindex repo data for repoKey: " + repoKey, e);
        }
        return repoData;
    }

    private SortedMap<String, RepoDataPackage> aggregateCurrent(SortedMap<String, RepoDataPackage> condaPackages) {
        Map<String, Map<String, RepoDataPackage>> condaPackageByName = CondaUtils.convertToMapByName(condaPackages);
        SortedMap<String, RepoDataPackage> condaStringRepoDataPackageMap = new TreeMap();
        for (Map.Entry<String, Map<String, RepoDataPackage>> entry : condaPackageByName.entrySet()) {
            String packageName = entry.getKey();
            Map<String, RepoDataPackage> repoDataPackages = entry.getValue();
            Map<String, RepoDataPackage> allLatestOfArtifact = this.getAllLatestOfArtifact(packageName, repoDataPackages);
            condaStringRepoDataPackageMap.putAll(allLatestOfArtifact);
        }
        return condaStringRepoDataPackageMap;
    }

    private List<Path> findCondaAndTarBz2Files(String repoKey) throws IOException {
        Path repoKeyPath = Paths.get(repoKey);
        if (!Files.exists(repoKeyPath)) {
            return List.of();
        }
        try (Stream<Path> stream = Files.walk(repoKeyPath)) {
            return stream
                    .filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".conda") || path.toString().endsWith(".tar.bz2"))
                    .collect(Collectors.toList());
        }
    }

}
