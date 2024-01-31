package com.veadan.folib.services;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.domain.FileContent;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.services.support.ArtifactRoutingRulesChecker;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.utils.compatator.DirectoryNameCompatator;
import lombok.Data;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public class DirectoryListingServiceImpl implements DirectoryListingService {

    private static final Logger logger = LoggerFactory.getLogger(DirectoryListingService.class);

    private String baseUrl;

    @Inject()
    @Named("asyncApiBrowseThreadPoolExecutor")
    private ThreadPoolTaskExecutor asyncApiBrowseThreadPoolExecutor ;
    public DirectoryListingServiceImpl(String baseUrl) {
        super();
        this.baseUrl = StringUtils.chomp(baseUrl.toString(), "/");
    }

    @Override
    public DirectoryListing fromStorages(Map<String, ? extends Storage> storages)
            throws IOException {
        DirectoryListing directoryListing = new DirectoryListing();

        for (Storage storage : storages.values()) {
            FileContent fileContent = new FileContent(storage.getId());
            directoryListing.getDirectories().add(fileContent);

            fileContent.setStorageId(storage.getId());
            fileContent.setUrl(calculateDirectoryUrl(fileContent));
        }

        return directoryListing;
    }

    @Override
    public DirectoryListing fromRepositories(Map<String, ? extends Repository> repositories)
            throws IOException {
        DirectoryListing directoryListing = new DirectoryListing();

        for (Repository repository : repositories.values()) {
            FileContent fileContent = new FileContent(repository.getId());
            directoryListing.getDirectories().add(fileContent);

            fileContent.setStorageId(repository.getStorage().getId());
            fileContent.setRepositoryId(repository.getId());

            fileContent.setUrl(calculateDirectoryUrl(fileContent));
        }

        return directoryListing;
    }

    @Override
    public DirectoryListing fromRepositoryPath(RepositoryPath path)
            throws IOException {
        return fromPath(path);
    }

    @Override
    public DirectoryListing fromGroupRepositoryPath(Repository repository, RepositoryPath path) throws IOException {
        ConfigurationManagementService configurationManagementService = SpringContextUtil.getBean(ConfigurationManagementService.class);
        RepositoryPathResolver repositoryPathResolver = SpringContextUtil.getBean(RepositoryPathResolver.class);
        ArtifactRoutingRulesChecker artifactRoutingRulesChecker = SpringContextUtil.getBean(ArtifactRoutingRulesChecker.class);
        List<RepositoryPath> hostedRepositoryPathList = Lists.newArrayList();
        List<RepositoryPath> proxyRepositoryPathList = Lists.newArrayList();
        for (String storageAndRepositoryId : repository.getGroupRepositories()) {
            String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
            String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
            Repository subRepository = configurationManagementService.getConfiguration().getRepository(sId, rId);
            if (!subRepository.isInService()) {
                continue;
            }
            RepositoryPath resolvedPath = repositoryPathResolver.resolve(subRepository, path);
            if (resolvedPath == null || !Files.exists(resolvedPath)) {
                continue;
            }
            if (artifactRoutingRulesChecker.isDenied(repository, resolvedPath)) {
                continue;
            }
            if (!repository.isAllowsDirectoryBrowsing() || !probeForDirectoryListing(resolvedPath)) {
                continue;
            }
            if (RepositoryTypeEnum.PROXY.getType().equals(subRepository.getType())) {
                proxyRepositoryPathList.add(resolvedPath);
            } else if (RepositoryTypeEnum.HOSTED.getType().equals(subRepository.getType())) {
                hostedRepositoryPathList.add(resolvedPath);
            }
        }
        List<DirectoryListing> directoryListingList = Lists.newArrayList();
        DirectoryListing directoryListing = null;
        for (RepositoryPath hostedRepositoryPath : hostedRepositoryPathList) {
            directoryListing = fromPath(hostedRepositoryPath);
            directoryListingList.add(directoryListing);
        }
        for (RepositoryPath proxyRepositoryPath : proxyRepositoryPathList) {
            directoryListing = fromPath(proxyRepositoryPath);
            directoryListingList.add(directoryListing);
        }
        Set<FileContent> directoryContentSet = Sets.newLinkedHashSet();
        Set<FileContent> fileContentSet = Sets.newLinkedHashSet();
        for (DirectoryListing itemDirectoryListing : directoryListingList) {
            directoryContentSet.addAll(itemDirectoryListing.getDirectories());
            fileContentSet.addAll(itemDirectoryListing.getFiles());
        }
        directoryListing = new DirectoryListing();
        List<FileContent> directoryContents = new ArrayList(directoryContentSet);
        Collections.sort(directoryContents, new DirectoryNameCompatator());
        directoryListing.setDirectories(directoryContents);
        directoryListing.setFiles(new ArrayList(fileContentSet));
        return directoryListing;
    }

    private DirectoryListing fromPath(Path path)
            throws IOException {
        path = path.normalize();

        DirectoryListing directoryListing = new DirectoryListing();

        Map<String, List<FileContent>> content = generateDirectoryListingV2(path);

        directoryListing.setDirectories(content.get("directories"));
        directoryListing.setFiles(content.get("files"));

        return directoryListing;
    }

    private Map<String, List<FileContent>> generateDirectoryListing(Path path)
            throws IOException {
        RepositoryPathResolver repositoryPathResolver = SpringContextUtil.getBean(RepositoryPathResolver.class);
        ConfigurationManagementService configurationManagementService = SpringContextUtil.getBean(ConfigurationManagementService.class);
        List<String> messageDigestAlgorithms = Lists.newArrayList(MessageDigestAlgorithms.MD5, MessageDigestAlgorithms.SHA_1, MessageDigestAlgorithms.SHA_256, MessageDigestAlgorithms.SHA_512);
        final boolean showChecksum = configurationManagementService.getConfiguration().getAdvancedConfiguration().isShowChecksum();
        List<FileContent> directories = new ArrayList<>();
        List<FileContent> files = new ArrayList<>();

        List<Path> contentPaths;
        try (Stream<Path> pathStream = Files.list(path)) {
            contentPaths = pathStream.filter(p -> !p.toString().startsWith("."))
                    .filter(p -> !p.toString().contains("/.")
                            // 支持Cocoapods索引目录的显示
                            || p.toString().contains(".specs")
                    )
                    .filter(p -> {
                        try {
                            return !Files.isHidden(p)
                                    // 支持Cocoapods索引目录的显示
                                    || p.toString().contains(".specs");
                        } catch (IOException e) {
                            logger.info("Error accessing path {}", p);
                            return false;
                        }
                    })
                    .sorted()
                    .collect(Collectors.toList());
        }

        PropertiesBooter propertiesBooter = SpringContextUtil.getBean(PropertiesBooter.class);
        for (Path contentPath : contentPaths) {
            FileContent file = new FileContent(contentPath.getFileName().toString());
            file.setPath(contentPath.toString().replace(propertiesBooter.getLogsDirectory().replace("./", ""), ""));
            Map<String, Object> fileAttributes = Files.readAttributes(contentPath, "*");

            file.setStorageId((String) fileAttributes.get(RepositoryFileAttributeType.STORAGE_ID.getName()));
            file.setRepositoryId((String) fileAttributes.get(RepositoryFileAttributeType.REPOSITORY_ID.getName()));

            file.setArtifactPath((String) fileAttributes.get("artifactPath"));
            boolean flag = !showChecksum && StringUtils.isNotBlank(file.getStorageId()) && StringUtils.isNotBlank(file.getRepositoryId()) && StringUtils.isNotBlank(file.getArtifactPath());
            if (flag) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(file.getStorageId(), file.getRepositoryId(), file.getArtifactPath());
                if (RepositoryFiles.isChecksum(repositoryPath)) {
                    continue;
                }
            }
            if (Boolean.TRUE.equals(fileAttributes.get("isDirectory"))) {
                file.setUrl(calculateDirectoryUrl(file));

                directories.add(file);

                continue;
            }
            file.setUrl((URL) fileAttributes.get(RepositoryFileAttributeType.RESOURCE_URL.getName()));

            file.setLastModified(new Date(((FileTime) fileAttributes.get("lastModifiedTime")).toMillis()));
            file.setSize((Long) fileAttributes.get("size"));

            files.add(file);
        }

        Map<String, List<FileContent>> listing = new HashMap<>();
        listing.put("directories", directories);
        listing.put("files", files);

        return listing;
    }

    private Map<String, List<FileContent>> generateDirectoryListingV2(Path path)
            throws IOException {
        RepositoryPathResolver repositoryPathResolver = SpringContextUtil.getBean(RepositoryPathResolver.class);
        ConfigurationManagementService configurationManagementService = SpringContextUtil.getBean(ConfigurationManagementService.class);
        final boolean showChecksum = configurationManagementService.getConfiguration().getAdvancedConfiguration().isShowChecksum();
        List<Path> contentPaths;
        try (Stream<Path> pathStream = Files.list(path)) {
            contentPaths = pathStream.filter(p -> !p.toString().startsWith("."))
                    .filter(p -> !p.toString().contains("/.")
                            // 支持Cocoapods索引目录的显示
                            || p.toString().contains(".specs")
                    )
                    .filter(p -> {
                        try {
                            return !Files.isHidden(p)
                                    // 支持Cocoapods索引目录的显示
                                    || p.toString().contains(".specs");
                        } catch (IOException e) {
                            logger.info("Error accessing path {}", p);
                            return false;
                        }
                    })
                    .sorted()
                    .collect(Collectors.toList());
        }

        PropertiesBooter propertiesBooter = SpringContextUtil.getBean(PropertiesBooter.class);

        BuildFileContentTaskResult result = new BuildFileContentTaskResult();
        List<List<Path>> partition = ListUtils.partition(contentPaths, 200);
        ArrayList<Callable<BuildFileContentTaskResult>> tasks = new ArrayList<>();

        for (List<Path> paths : partition) {
            tasks.add(() -> buildFileContentTask(paths, propertiesBooter, showChecksum, repositoryPathResolver));
        }
        try {
            List<Future<BuildFileContentTaskResult>> results = asyncApiBrowseThreadPoolExecutor.getThreadPoolExecutor().invokeAll(tasks);
            for (Future<BuildFileContentTaskResult> object : results) {
                BuildFileContentTaskResult buildFileContentTaskResult = object.get();
                result.merge(buildFileContentTaskResult);
            }
        } catch (InterruptedException | ExecutionException e) {
            throw new BusinessException(e.getMessage());
        }

        List<FileContent> directories = result.getDirectories();
        List<FileContent> files = result.getFiles();

        Map<String, List<FileContent>> listing = new HashMap<>();
        listing.put("directories", directories);
        listing.put("files", files);

        return listing;
    }

    private BuildFileContentTaskResult buildFileContentTask(List<Path> contentPaths,
                                                            PropertiesBooter propertiesBooter,
                                                            boolean showChecksum,
                                                            RepositoryPathResolver repositoryPathResolver
    ) throws IOException  {
        BuildFileContentTaskResult result = new BuildFileContentTaskResult();
        List<FileContent> directories = result.getDirectories();
        List<FileContent> files = result.getFiles();
        for (Path contentPath : contentPaths) {
            FileContent file = new FileContent(contentPath.getFileName().toString());
            file.setPath(contentPath.toString().replace(propertiesBooter.getLogsDirectory().replace("./", ""), ""));
            Map<String, Object> fileAttributes = Files.readAttributes(contentPath, "*");

            file.setStorageId((String) fileAttributes.get(RepositoryFileAttributeType.STORAGE_ID.getName()));
            file.setRepositoryId((String) fileAttributes.get(RepositoryFileAttributeType.REPOSITORY_ID.getName()));

            file.setArtifactPath((String) fileAttributes.get("artifactPath"));
            boolean flag = !showChecksum && StringUtils.isNotBlank(file.getStorageId()) && StringUtils.isNotBlank(file.getRepositoryId()) && StringUtils.isNotBlank(file.getArtifactPath());
            if (flag) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(file.getStorageId(), file.getRepositoryId(), file.getArtifactPath());
                if (RepositoryFiles.isChecksum(repositoryPath)) {
                    continue;
                }
            }
            if (Boolean.TRUE.equals(fileAttributes.get("isDirectory"))) {
                file.setUrl(calculateDirectoryUrl(file));
                directories.add(file);
                continue;
            }
            file.setUrl((URL) fileAttributes.get(RepositoryFileAttributeType.RESOURCE_URL.getName()));

            file.setLastModified(new Date(((FileTime) fileAttributes.get("lastModifiedTime")).toMillis()));
            file.setSize((Long) fileAttributes.get("size"));
            files.add(file);
        }
        return result;
    }

    /**
     * @param rootPath The root path in which directory listing is allowed. Used as a
     *                 precaution to prevent directory traversing.
     *                 When "path" is outside "rootPath" an exception will be thrown.
     * @param path     The path which needs to be listed
     * @return DirectoryListing
     * @throws RuntimeException when path is not within rootPath.
     */
    @Override
    public DirectoryListing fromPath(Path rootPath,
                                     Path path)
            throws IOException {
        rootPath = rootPath.normalize();
        path = path.normalize();

        if (!path.equals(rootPath) && !path.startsWith(rootPath)) {
            String message = String.format(
                    "Requested directory listing for [%s] is outside the scope of the root path [%s]! Possible intrusion attack or misconfiguration!",
                    path, rootPath);
            logger.error(message);
            throw new RuntimeException(message);
        }

        return fromPath(path);
    }

    private URL calculateDirectoryUrl(FileContent file)
            throws MalformedURLException {
        if (file.getRepositoryId() == null) {

            return new URL(String.format("%s/%s", baseUrl, file.getStorageId()));

        } else if (file.getArtifactPath() == null) {

            return new URL(String.format("%s/%s/%s", baseUrl, file.getStorageId(),
                    file.getRepositoryId()));

        }

        return new URL(String.format("%s/%s/%s/%s", baseUrl, file.getStorageId(),
                file.getRepositoryId(), file.getArtifactPath()));
    }

    protected boolean probeForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException {
        return Files.exists(repositoryPath) &&
                repositoryPath.getRepository().getLayout().equals("helm") && repositoryPath.getTarget().toString().endsWith("index.yaml") || Files.isDirectory(repositoryPath) &&
                isPermittedForDirectoryListing(repositoryPath);
    }

    protected boolean isPermittedForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException {
        //TODO: RepositoryFiles.isIndex(repositoryPath) || (
        return !Files.isHidden(repositoryPath) && !RepositoryFiles.isTrash(repositoryPath)
                && !RepositoryFiles.isTemp(repositoryPath);
    }

    @Data
    static class BuildFileContentTaskResult {
        List<FileContent> directories = new ArrayList<>();
        List<FileContent> files = new ArrayList<>();

        public BuildFileContentTaskResult merge(BuildFileContentTaskResult result) {
            directories.addAll(result.getDirectories());
            files.addAll(result.getFiles());
            return this;
        }
    }
}
