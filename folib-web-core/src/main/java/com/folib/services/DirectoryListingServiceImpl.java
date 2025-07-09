/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.services;

import cn.hutool.core.date.StopWatch;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.folib.booters.PropertiesBooter;
import com.folib.components.DistributedCacheComponent;
import com.folib.components.auth.AuthComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.constant.GlobalConstants;
import com.folib.domain.DirectoryListing;
import com.folib.domain.FileContent;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.io.RootRepositoryPath;
import com.folib.providers.io.StorageFileSystemProvider;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.scanner.common.exception.BusinessException;
import com.folib.scanner.common.util.SpringContextUtil;
import com.folib.services.support.ArtifactRoutingRulesChecker;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.users.domain.Privileges;
import com.folib.utils.compatator.DirectoryNameCompatator;
import lombok.Data;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.nio.file.spi.FileSystemProvider;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public class DirectoryListingServiceImpl implements DirectoryListingService {

    private static final Logger logger = LoggerFactory.getLogger(DirectoryListingService.class);

    private String baseUrl;
    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Inject()
    @Named("asyncApiBrowseThreadPoolExecutor")
    private ThreadPoolTaskExecutor asyncApiBrowseThreadPoolExecutor;

    @Autowired
    @Lazy
    private AuthComponent authComponent;

    @Autowired
    @Lazy
    private LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    public DirectoryListingServiceImpl(String baseUrl) {
        super();
        this.baseUrl = StringUtils.chomp(baseUrl.toString(), "/");
    }

    @Override
    public DirectoryListing fromStorages(Map<String, ? extends Storage> storages)
            throws IOException {
        DirectoryListing directoryListing = new DirectoryListing();

        for (Storage storage : storages.values()) {
            boolean result = authComponent.validateStoragePrivileges(storage.getId(), Privileges.ARTIFACTS_RESOLVE.getAuthority());
            if (!result) {
                continue;
            }
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
            RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository);
            boolean result = authComponent.validatePrivileges(repository, rootRepositoryPath, Privileges.ARTIFACTS_RESOLVE.getAuthority());
            if (!result) {
                continue;
            }
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
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        ConfigurationManager configurationManager = SpringContextUtil.getBean(ConfigurationManager.class);
        configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
            String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
            Repository subRepository = configurationManagementService.getConfiguration().getRepository(sId, rId);
            if (!authComponent.validatePrivilegesSplitPath(subRepository, path, Privileges.ARTIFACTS_RESOLVE.getAuthority())) {
                continue;
            }
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
        if (!Files.exists(path)) {
            return directoryListing;
        }
        //Map<String, List<FileContent>> content = generateDirectoryListingV2(path);
        StopWatch stopWatch = new StopWatch();
        stopWatch.start("fromPath");
        Map<String, List<FileContent>> content = generateDirectoryListingV3(path).block();

        directoryListing.setDirectories(content.get("directories"));
        directoryListing.setFiles(content.get("files"));
        stopWatch.stop();
        logger.info("generateDirectoryListingV3: " + stopWatch.getTotalTimeMillis() + "ms");
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
            contentPaths = pathStream.filter(p -> !p.toString().startsWith(".") && !DockerLayoutProvider.MANIFEST.equalsIgnoreCase(p.getFileName().toString()) && !DockerLayoutProvider.BLOBS.equalsIgnoreCase(p.getFileName().toString()))
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
                file.setLastModified(new Date(((FileTime) fileAttributes.get("lastModifiedTime")).toMillis()));
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
            contentPaths = pathStream.filter(p -> !p.toString().startsWith(".") && !DockerLayoutProvider.MANIFEST.equalsIgnoreCase(p.getFileName().toString()) && !DockerLayoutProvider.BLOBS.equalsIgnoreCase(p.getFileName().toString()))
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
        StopWatch stopWatch2 = new StopWatch("generateDirectoryListingV2-task-2");
        stopWatch2.start("generateDirectoryListingV2-task-2");
        for (List<Path> paths : partition) {
            tasks.add(() -> buildFileContentTask(paths, propertiesBooter, showChecksum, repositoryPathResolver));
        }
        try {
            List<Future<BuildFileContentTaskResult>> results = asyncApiBrowseThreadPoolExecutor.getThreadPoolExecutor().invokeAll(tasks);
            for (Future<BuildFileContentTaskResult> object : results) {
                BuildFileContentTaskResult buildFileContentTaskResult = object.get();
                result.merge(buildFileContentTaskResult);
            }
        } catch (Exception e) {
            logger.error(ExceptionUtils.getStackTrace(e));
            throw new BusinessException(e.getMessage());
        }
        List<FileContent> directories = result.getDirectories();
        List<FileContent> files = result.getFiles();

        Map<String, List<FileContent>> listing = new HashMap<>();
        listing.put("directories", directories);
        listing.put("files", files);

        return listing;
    }

    /**
     * 生成目录列表
     *
     * @param path
     * @return
     */
    public Mono<Map<String, List<FileContent>>> generateDirectoryListingV3(Path path) {
        // 获取配置管理服务，用于后续获取配置信息
        ConfigurationManagementService configurationManagementService = SpringContextUtil.getBean(ConfigurationManagementService.class);
        // 从配置中获取是否显示校验和的设置
        boolean showChecksum = configurationManagementService.getConfiguration().getAdvancedConfiguration().isShowChecksum();
        return listPaths(path)
                //.filter(this::isValidPath)
                .collectList()
                .flatMapMany(contentPaths -> Flux.fromIterable(ListUtils.partition(contentPaths, this.getRepositoryPathBatchSize())))
                // 使用并行处理提高效率 在多个 Flux 中使用 .publishOn(Schedulers.boundedElastic()) 时，Schedulers.boundedElastic() 是共享的，而不是为每个 Flux 创建独立的线程池。
                .publishOn(Schedulers.boundedElastic())
                .parallel(this.getRepositoryPathThread())
                // 根据路径构建文件内容任务
                .flatMap(paths -> buildFileContentTask(paths, showChecksum))
                // 将并行处理的结果序列化
                .sequential()
                .collectList()
                // 合并处理结果
                .map(this::mergeResults);

    }

    /**
     * 使用Reactive Streams API处理文件列表。
     * <p>
     * 此段代码利用Flux.using方法来管理文件目录的流式处理。它在反应式流中封装了文件系统的操作，
     * 以便以非阻塞方式遍历指定路径下的文件列表。这种方式特别适用于需要处理大量文件或目录的情况，
     * 可以提高程序的并发性和效率。
     *
     * @param path 要遍历的文件目录路径。
     * @return 返回一个Flux实例，该实例用于异步遍历并处理目录中的文件。
     */
    private Flux<Path> listPaths(Path path) {
        Flux<Path> pathFlux = Flux.using(
                //Files.list 无法保证所有文件系统中的顺序一致性,需要自定义顺序，必须显式调用 .sorted() 自然排序
                () -> Files.list(path).filter(this::isValidPath).sorted(),
                Flux::fromStream, // 将列出的文件转换为Flux流，以便进行反应式处理。
                Stream::close // 指定在不再需要流时如何关闭它，确保资源的正确释放。
        ); // 指定在哪个线程上订阅和处理事件，这里选择使用bounded
        return pathFlux;
    }

    /**
     * 判断给定的路径是否为有效的文件路径。
     * 该方法用于过滤掉不符合要求的路径，包括但不限于隐藏文件、特殊文件名以及不符合路径规范的文件。
     *
     * @param p 待检查的Path对象，代表一个文件或目录的路径。
     * @return 如果路径有效，则返回true；否则返回false。
     */
    private boolean isValidPath(Path p) {
        // 检查路径字符串是否以点(.)开头，或者文件名是否为MANIFEST或BLOBS，这些都不被认为是有效的层文件。
        // 同时检查路径中是否包含'/.'但不包含'.specs'，或者文件是隐藏文件但路径中不包含'.specs'，这些情况都会被认为是无效路径。
        try {
            boolean isValid = (!p.toString().startsWith(".")) &&
                    !DockerLayoutProvider.MANIFEST.equalsIgnoreCase(p.getFileName().toString()) &&
                    !DockerLayoutProvider.BLOBS.equalsIgnoreCase(p.getFileName().toString()) &&
                    (!p.toString().contains("/.") || p.toString().contains(".specs")) &&
                    (!Files.isHidden(p) || p.toString().contains(".specs"));
            if (isValid) {
                //校验权限
                if (p instanceof RepositoryPath) {
                    RepositoryPath repositoryPath = (RepositoryPath) p;
                    if (!authComponent.validatePrivilegesSplitPath(repositoryPath.getRepository(), repositoryPath, Privileges.ARTIFACTS_RESOLVE.getAuthority())) {
                        return false;
                    }
                }
            }
            return isValid;
        } catch (IOException e) {
            logger.info("Error accessing path {}", p);
            return false;
        }
    }

    /**
     * 构建文件内容任务结果。
     * <p>
     * 该方法通过遍历给定的文件路径列表，收集每个文件或目录的信息，并包装成FileContent对象。
     * 根据是否显示校验和，决定是否忽略某些文件。最终返回包含所有文件内容信息的结果对象。
     *
     * @param contentPaths 文件路径列表，用于收集文件内容信息。
     * @param showChecksum 是否显示校验和，此参数用于控制是否忽略某些特定的文件。
     * @return 返回一个Mono对象，包含构建的文件内容任务结果。
     */
    private Mono<BuildFileContentTaskResult> buildFileContentTask(List<Path> contentPaths, boolean showChecksum) {
        // 通过SpringContextUtil获取Beans，用于后续操作。
        PropertiesBooter propertiesBooter = SpringContextUtil.getBean(PropertiesBooter.class);
        ConfigurationManagementService configurationManagementService = SpringContextUtil.getBean(ConfigurationManagementService.class);
        RepositoryPathResolver repositoryPathResolver = SpringContextUtil.getBean(RepositoryPathResolver.class);

        // 使用Mono.fromCallable将后续操作包装为一个Callable任务，并指定在boundedElastic线程池中执行。
        return Mono.fromCallable(() -> {
            BuildFileContentTaskResult result = new BuildFileContentTaskResult();
            List<FileContent> directories = result.getDirectories();
            List<FileContent> files = result.getFiles();

            // 遍历内容路径列表，对每个路径收集文件或目录信息。
            for (Path contentPath : contentPaths) {
                FileContent file = new FileContent(contentPath.getFileName().toString());
                // 设置文件路径，移除日志目录前缀。
                file.setPath(contentPath.toString().replace(propertiesBooter.getLogsDirectory().replace("./", ""), ""));
                //StopWatch stopWatch = new StopWatch();
                //stopWatch.start();
                // 读取文件或目录的属性。
                FileSystemProvider provider = contentPath.getFileSystem().provider();
                Map<String, Object> fileAttributes = null;
                if (provider instanceof StorageFileSystemProvider) {
                    fileAttributes = Files.readAttributes(contentPath, "folib:repositoryId,folib:storageId,folib:artifactPath,folib:resourceUrl,lastModifiedTime,size,isDirectory");
                } else {
                    fileAttributes = Files.readAttributes(contentPath, "*");
                }
                //if(contentPath.toString().startsWith("s3://") || contentPath.toString().startsWith("https://s3.")){
                //    // 读取文件或目录的属性。
                //    fileAttributes = Files.readAttributes(contentPath, "folib:repositoryId,folib:storageId,folib:artifactPath,folib:resourceUrl,lastModifiedTime,size,isDirectory");
                //    //fileAttributes = Files.readAttributes(contentPath, "*");
                ////Files.readAttributes(contentPath,"artifactPath,lastModifiedTime,resourceUrl,isDirectory,folib:repositoryId,size,folib:storageId");
                //}else {
                //    fileAttributes = Files.readAttributes(contentPath, "folib:repositoryId,folib:storageId,folib:artifactPath,folib:resourceUrl,lastModifiedTime,size,isDirectory");
                //}
                //stopWatch.stop();
                //logger.info("Read file attributes for {} in {} ms", file.getName(), stopWatch.getTotalTimeMillis());
                // 从文件属性中提取存储ID、仓库ID和artifact路径，并设置到FileContent对象中。
                file.setStorageId((String) fileAttributes.get(RepositoryFileAttributeType.STORAGE_ID.getName()));
                file.setRepositoryId((String) fileAttributes.get(RepositoryFileAttributeType.REPOSITORY_ID.getName()));
                file.setArtifactPath((String) fileAttributes.get("artifactPath"));

                // 根据是否显示校验和以及文件是否具有必要的属性，决定是否忽略该文件。
                boolean flag = !showChecksum && StringUtils.isNotBlank(file.getStorageId()) && StringUtils.isNotBlank(file.getRepositoryId()) && StringUtils.isNotBlank(file.getArtifactPath());
                if (flag) {
                    RepositoryPath repositoryPath = repositoryPathResolver.resolve(file.getStorageId(), file.getRepositoryId(), file.getArtifactPath());
                    if (RepositoryFiles.isChecksum(repositoryPath)) {
                        continue;
                    }
                }

                // 判断文件是否为目录，如果是，计算其URL并添加到目录列表中。
                if (Boolean.TRUE.equals(fileAttributes.get("isDirectory"))) {
                    file.setUrl(calculateDirectoryUrl(file));
                    file.setLastModified(new Date(((FileTime) fileAttributes.get("lastModifiedTime")).toMillis()));
                    directories.add(file);
                    continue;
                }

                // 对于文件，设置其URL、最后修改时间和大小，并添加到文件列表中。
                file.setUrl((URL) fileAttributes.get(RepositoryFileAttributeType.RESOURCE_URL.getName()));
                file.setLastModified(new Date(((FileTime) fileAttributes.get("lastModifiedTime")).toMillis()));
                file.setSize((Long) fileAttributes.get("size"));
                files.add(file);
            }
            return result;
        }).subscribeOn(Schedulers.boundedElastic());
    }

    private Map<String, List<FileContent>> mergeResults(List<BuildFileContentTaskResult> results) {
        BuildFileContentTaskResult finalResult = new BuildFileContentTaskResult();
        for (BuildFileContentTaskResult result : results) {
            finalResult.merge(result);
        }
        Map<String, List<FileContent>> listing = new HashMap<>();
        finalResult.getDirectories().sort(Comparator.comparing(FileContent::getName));
        finalResult.getFiles().sort(Comparator.comparing(FileContent::getName));
        listing.put("directories", finalResult.getDirectories());
        listing.put("files", finalResult.getFiles());
        return listing;
    }


    private BuildFileContentTaskResult buildFileContentTask(List<Path> contentPaths,
                                                            PropertiesBooter propertiesBooter,
                                                            boolean showChecksum,
                                                            RepositoryPathResolver repositoryPathResolver
    ) throws IOException {
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
        if (StringUtils.isBlank(file.getRepositoryId())) {

            return new URL(String.format("%s/%s", baseUrl, file.getStorageId()));

        } else if (StringUtils.isBlank(file.getArtifactPath())) {

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
        return !Files.isHidden(repositoryPath) && !RepositoryFiles.isTemp(repositoryPath);
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

    public int getRepositoryPathThread() {
        int thread = GlobalConstants.REPOSITORY_PATH_THREAD;
        String cacheKey = distributedCacheComponent.get(GlobalConstants.REPOSITORY_PATH_THREAD_KEY);
        if (StringUtils.isNotBlank(cacheKey)) {
            thread = Integer.parseInt(cacheKey);
        }
        return thread;

    }

    public int getRepositoryPathBatchSize() {
        int batchSize = GlobalConstants.REPOSITORY_PATH_BATCH_SIZE;
        String cacheKey = distributedCacheComponent.get(GlobalConstants.REPOSITORY_PATH_BATCH_SIZE_KEY);
        if (StringUtils.isNotBlank(cacheKey)) {
            batchSize = Integer.parseInt(cacheKey);
        }
        return batchSize;
    }


}
