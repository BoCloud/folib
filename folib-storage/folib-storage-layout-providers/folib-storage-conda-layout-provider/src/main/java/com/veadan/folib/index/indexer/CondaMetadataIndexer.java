package com.veadan.folib.index.indexer;

import com.google.common.annotations.VisibleForTesting;
import com.veadan.folib.index.model.*;
import com.veadan.folib.index.utils.CondaUtils;
import com.veadan.folib.index.utils.CondaVersionComparator;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;


/**
 * @author LingengMa
 * @date 2025/04/11 10:52
 * @Description: 负责获取包的元数据, 以及获取和构建目录索引(增, 删)
 */

@Component
public class CondaMetadataIndexer {
    private CondaMetadataExtractor extractor;

    public CondaMetadataIndexer(CondaMetadataExtractor extractor) {
        this.extractor = extractor;
    }

    @VisibleForTesting
    void setCondaMetadataExtractor(CondaMetadataExtractor extractor) {
        this.extractor = extractor;
    }

    @Autowired
    RepositoryPathResolver repositoryPathResolver;




    /**
     * @Description: 将包转化为包索引数据
     * @param repoKey
     * @param artifactName
     * @return
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
                this.supplementRepoDataPackage(repoDataPackage, repoKey, artifactName);
                return repoDataPackage;
            }
        }
    }

    /**
     * @param parentPath
     * @return
     * @Description: 创建新的索引数据
     */
    public RepoData createNewRepoData(String parentPath) {
        RepoData repoData = new RepoData();
        repoData.setInfo(new RepoDataInfo());
        String subdir;
        if (parentPath.lastIndexOf(47) != -1) {
            subdir = StringUtils.substringAfterLast(parentPath, "/");
        } else {
            subdir = parentPath;
        }

        repoData.getInfo().setSubdir(subdir);
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


    public boolean checkPackageExistsInRepoData(RepoData repoData,
                                                String artifactName) {
        SortedMap<String, RepoDataPackage> packageSortedMap = CondaUtils.isTarBz2File(artifactName) ? repoData.getPackages() : repoData.getCondaPackages();
        return packageSortedMap.containsKey(artifactName);
    }

    private void supplementRepoDataPackage(RepoDataPackage repoDataPackage, String repoKey,
                                           @NonNull String artifactName) {
        repoDataPackage.setSize(123L);
        repoDataPackage.setMd5("123");
        repoDataPackage.setSha256("1234");
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
     * @Description: 重新索引当前的索引数据
     * @param repoData
     * @param packagesParentPath
     * @return
     */
    @Nullable
    public RepoData reindexCurrentRepoData(RepoData repoData, String packagesParentPath) {
        if (repoData == null) {
            return null;
        } else {
            RepoData currentRepoData = this.createNewRepoData(packagesParentPath);
            currentRepoData.setInfo(repoData.getInfo());
            SortedMap<String, RepoDataPackage> tgzRepoDataPackageMap = this.aggregateCurrent(repoData.getPackages());
            currentRepoData.setPackages(tgzRepoDataPackageMap);
            SortedMap<String, RepoDataPackage> condaRepoDataPackageMap = this.aggregateCurrent(repoData.getCondaPackages());
            currentRepoData.setCondaPackages(condaRepoDataPackageMap);
            return currentRepoData;
        }
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


}
