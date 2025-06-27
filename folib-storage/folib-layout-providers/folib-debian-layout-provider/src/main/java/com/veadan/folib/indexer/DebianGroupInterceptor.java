package com.veadan.folib.indexer;

import com.google.common.base.Joiner;
import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.domain.DebianGroupMetadataWorkItem;
import com.veadan.folib.domain.DebianReleaseContext;
import com.veadan.folib.interceptor.GroupInterceptor;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.DebianUtils;
import com.veadan.folib.utils.PathUtils;
import lombok.extern.slf4j.Slf4j;
import org.checkerframework.checker.units.qual.A;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StopWatch;

import java.nio.file.InvalidPathException;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author veadan
 * @since 2025-02-23 10:46
 */
@Slf4j
@Component
public class DebianGroupInterceptor implements GroupInterceptor {

    private final DebianGroupHelper debianGroupHelper;

    private final RepositoryPathResolver repositoryPathResolver;


    @Autowired
    public DebianGroupInterceptor(DebianGroupHelper debianGroupHelper,RepositoryPathResolver repositoryPathResolver) {
        this.debianGroupHelper = debianGroupHelper;
        this.repositoryPathResolver = repositoryPathResolver;
    }

    @Override
    public boolean shouldInterceptor(RepositoryPath path) {
        return this.shouldTakeAction(path);
    }

    @Override
    public void calculateIndex(RepositoryPath path) {

        if(!isReleaseFile(path)){
            return;
        }

        Repository repository = path.getRepository();
        String distribution = DebianUtils.getDistributionName(path);
        String component = getComponentFromPackagesOrReleaseRequest(path);
        DebianGroupMetadataWorkItem debianGroupMetadataWorkItem = new DebianGroupMetadataWorkItem(repository, distribution, component);
        createOrUpdateVirtualIndexes(debianGroupMetadataWorkItem);
    }


    private String getComponentFromPackagesOrReleaseRequest(RepositoryPath packagesFileRepoPath) {
        return packagesFileRepoPath.getName().contains("Packages") ? DebianUtils.getComponentFromPackagesRepoPath(packagesFileRepoPath) : "";
    }

//    private RepoResource retrieveResourceFromVirtualCache(VirtualRepo repo, InternalRequestContext context) {
//        try {
//            RepoResource cachedResource = repo.getInfoFromVirtualCache(context);
//            if (this.cachedResourceExists(cachedResource)) {
//                return cachedResource;
//            }
//        } catch (Exception var5) {
//            String resourcePath = context.getResourcePath();
//            Logger var10000 = log;
//            String var10003 = repo.getKey();
//            var10000.trace("Attempt to locate resource {} in cache failed, executing calculation of virtual debian metadata on path {}", resourcePath, var10003 + "/" + resourcePath);
//        }
//
//        return null;
//    }

//    @Nonnull
//    public RepoResource onBeforeReturn(Repository virtualRepo, InternalRequestContext context, RepoResource resource) {
//        if (RepoType.Debian.equals(((VirtualRepoConfig)virtualRepo.getRepoConfig()).getPackageType())) {
//            String path = resource.getRepoPath().getPath();
//            if (isReleaseRequestWithNoPrivateKey(path)) {
//                return new UnfoundRepoResource(resource.getRepoPath(), resource.getRepoPath().getName() + " Not exist");
//            }
//        }
//        return resource;
//    }

     boolean isReleaseRequestWithNoPrivateKey(String path,Repository repository) {
        return (isInReleaseFile(path) || isReleaseGpgFile(path)) && !hasPrivateKey(repository);
    }

    private boolean shouldTakeAction(RepositoryPath path) {
        // 1.判断是否为debian仓库 2.判断是否请求的元数据
        String layout = path.getRepository().getLayout();
        String type = path.getRepository().getType();

        return DebianConstant.LAYOUT_NAME.equals(layout) &&"group".equals(type) &&isMetadataRequest(path);
    }

    private boolean isByHash(String path) {
        return path.contains("by-hash");
    }

    private  boolean isDebFile(String path) {
        return path.matches(".*([.][d]?deb$)");
    }

    boolean isMetadataRequest(RepositoryPath repositoryPath) {

        RootRepositoryPath rootDirectory = repositoryPath.getFileSystem().getRootDirectory();
        RepositoryPath relativePath = rootDirectory.relativize(repositoryPath);;
        String path=relativePath.getPath();
        if (path == null) {
            return false;
        } else if (path.contains("dists/by-hash")) {
            return false;
        } else {
            String lastPathElement = PathUtils.getLastPathElement(path);
            return lastPathElement.contains("Packages") || isReleaseFileInProperLevel(path) || isReleaseGpgFile(path) || isInReleaseFile(path);
        }
    }

    private  boolean isReleaseFileInProperLevel(String path) {
        String[] pathElements = PathUtils.getPathElements(path);
        return pathElements.length == 3 && pathElements[2].equalsIgnoreCase("Release");
    }

    private  boolean isReleaseGpgFile(String path) {
        String lastPathElement = PathUtils.getLastPathElement(path);
        return lastPathElement.equalsIgnoreCase("Release.gpg");
    }

    private  boolean isInReleaseFile(String path) {
        String lastPathElement = PathUtils.getLastPathElement(path);
        return lastPathElement.equalsIgnoreCase("InRelease");
    }

    private  boolean isReleaseFile(RepositoryPath path) {
        String lastPathElement = PathUtils.getLastPathElement(path.toString());
        return lastPathElement.equalsIgnoreCase("Release");
    }

//    private boolean cachedResourceExists(RepoResource resource) {
//        return resource != null && (resource.isFound() || resource.isExpired());
//    }

    // 判断仓库是否支持公私钥 目前不支持后期实现
    private  boolean hasPrivateKey(Repository repository) {
        return false;
    }


    private void createOrUpdateVirtualIndexes(DebianGroupMetadataWorkItem workItem) {
        String distribution = workItem.getDistribution();
        Repository groupRepo = workItem.getRepository();
        if (groupRepo == null) {
            log.info("Unable to find repository {}", groupRepo.getId());
        } else {
            DebianGroupIndexesCollector debianVirtualIndexesCollector = this.getGroupIndexesCollector(workItem);
//            debianVirtualIndexesCollector.collectPackagesFilesToRecalculate(workItem);
            this.indexVirtualRepo(debianVirtualIndexesCollector);
            log.info("Finished writing automatic Debian index for {}/{}", groupRepo.getId(), distribution);
            this.debianGroupHelper.validateParentVirtualRepository(workItem, groupRepo);
        }
    }

//    public void unexpireMetadataFiles(Repository groupRepo, String distribution, String component) {
//        log.debug("Unexpiring Packages/Release files under distribution {}", distribution);
//        this.debianVirtualHelper.unexpirePackagesFiles(groupRepo, distribution, component);
//        this.debianVirtualHelper.unexpireReleaseFile(groupRepo, distribution);
//        log.debug("Finished unexpiring Packages/Release files under distribution {}", distribution);
//    }

    private DebianGroupIndexesCollector getGroupIndexesCollector(DebianGroupMetadataWorkItem workItem) {
        String distribution = workItem.getDistribution();
        String component = workItem.getComponent();
        Repository groupRepo = workItem.getRepository();
        log.info("Starting to writing automatic Debian index for {}/{}", groupRepo.getId(), distribution);
        DebianGroupIndexesCollector debianVirtualIndexesCollector = new DebianGroupIndexesCollector(groupRepo, distribution, component);
//        DpkgUtils.updateSigningPassword((DpkgWorkContext)debianVirtualIndexesCollector.getRepo().getWorkContext(), workItem.getPassphrase());
        return debianVirtualIndexesCollector;
    }

    private void indexVirtualRepo(DebianGroupIndexesCollector debianVirtualIndexesCollector) {
        String distribution = debianVirtualIndexesCollector.getDistribution();
        DebianRepoGroupMetadataIndexFinalizer finalizer = new DebianRepoGroupMetadataIndexFinalizer(debianVirtualIndexesCollector.getGroupRepo(), distribution, null);
        try {
            StopWatch stopWatch = new StopWatch();
            stopWatch.start();
            this.indexPackages(debianVirtualIndexesCollector);
            this.indexRelease(debianVirtualIndexesCollector);
            finalizer.setCompFoldersToDelete(debianVirtualIndexesCollector.getCompFoldersToDelete());
            finalizer.finalizeIndex();
            stopWatch.stop();
            log.info("Finished index for all required packages of distribution {}, took {} s", distribution, stopWatch.getTotalTimeSeconds());
        } finally {
            finalizer.removeTemp();
        }

    }


    private void indexPackages(DebianGroupIndexesCollector debianGroupIndexesCollector) {
        Map<String, Map<String, List<CharSequence>>> componentsToArchsToPackagesFileContent = debianGroupIndexesCollector.collectPackagesFilesFromAggregatedRepos();
        Map<String, Map<String, String>> compToArchToMergedPackageFile = this.debianGroupHelper.mergePackagesFilesByArch(componentsToArchsToPackagesFileContent);
        this.debianGroupHelper.persistMergedPackageFiles(debianGroupIndexesCollector, compToArchToMergedPackageFile);
    }


    private void indexRelease(DebianGroupIndexesCollector debianGroupIndexesCollector) {
        Repository groupRepo = debianGroupIndexesCollector.getGroupRepo();
        String distribution = debianGroupIndexesCollector.getDistribution();
        debianGroupIndexesCollector.copyMissingPackagesFromVirtualCacheToVirtualCacheTempFolder();
        DebianReleaseMetadataIndexer releaseIndexer = new DebianReleaseMetadataIndexer(groupRepo, null,this.repositoryPathResolver);
        Set<String> componentsInTempPath = new HashSet<>();
        Set<String> archsInTempPath = new HashSet<>();
        this.debianGroupHelper.extractExistingArchsAndCompsInTempPath(groupRepo, debianGroupIndexesCollector, componentsInTempPath, archsInTempPath);
        releaseIndexer.indexRelease(distribution);
    }

    private boolean isNeedToRecalculateRepo(DebianGroupIndexesCollector debianGroupIndexesCollector) {
        return true;
//        log.debug("Validating if Release file needed recalculate");
//        Repository groupRepo = debianGroupIndexesCollector.getGroupRepo();
//        String distribution = debianGroupIndexesCollector.getDistribution();
//        RepositoryPath releaseFileRepoPath = repositoryPathResolver.resolve(groupRepo, Joiner.on("/").join("dists", distribution,"Release"));
//        RepoResource releaseFileResource = virtualRepo.getInfoFromVirtualCache(DebianUtils.getDownloadRequestContext(releaseFileRepoPath));
//        if (!releaseFileResource.isFound()) {
//            log.debug("Release file not found in {}, forcing recalculate Release/Packages files", releaseFileRepoPath);
//            return true;
//        } else {
//            Properties props = this.debianVirtualHelper.getPropertiesAndValidateNotEmpty(releaseFileResource.getRepoPath());
//            if (props.isEmpty()) {
//                log.debug("Couldn't find properties on Release file:{}, forcing recalculate Release/Packages files", releaseFileResource.getRepoPath());
//                return true;
//            } else {
//                log.debug("checking SHA1 mismatch on Release file in path:{}, with props:{}, ", releaseFileRepoPath, props);
//                return this.debianVirtualHelper.isReleaseFileSha1Mismatch(virtualRepo, distribution, props) || debianVirtualIndexesCollector.isNeedToRecalculatePackagesFile();
//            }
//        }
    }


}
