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
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.components.ArtifactSecurityComponent;
import com.folib.components.DistributedCacheComponent;
import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.enums.ProductTypeEnum;
import com.folib.event.artifact.ArtifactEventListenerRegistry;
import com.folib.io.LayoutInputStream;
import com.folib.io.LayoutOutputStream;
import com.folib.io.StreamUtils;
import com.folib.providers.ProviderImplementationException;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.io.RepositoryStreamSupport;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.repositories.ArtifactRepository;
import com.folib.storage.ArtifactResolutionException;
import com.folib.storage.ArtifactStorageException;
import com.folib.storage.Storage;
import com.folib.storage.checksum.ArtifactChecksum;
import com.folib.storage.checksum.ChecksumCacheManager;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.storage.validation.ArtifactCoordinatesValidator;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;
import com.folib.storage.validation.artifact.version.VersionValidationException;
import com.folib.storage.validation.deployment.RedeploymentValidator;
import com.folib.storage.validation.resource.ArtifactOperationsValidator;
import com.folib.users.domain.Privileges;
import com.folib.util.DirectoryValidatorUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.FileSystemUtils;

import jakarta.inject.Inject;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Component
public class ArtifactManagementService {
    private static final Logger logger = LoggerFactory.getLogger(ArtifactManagementService.class);

    @Inject
    protected ArtifactOperationsValidator artifactOperationsValidator;

    @Inject
    protected ArtifactCoordinatesValidatorRegistry artifactCoordinatesValidatorRegistry;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    protected ArtifactRepository artifactEntityRepository;

    @Inject
    protected LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    protected ChecksumCacheManager checksumCacheManager;

    @Inject
    protected ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Autowired
    @Lazy
    protected ArtifactSecurityComponent artifactSecurityComponent;

    @Autowired
    private DistributedCacheComponent distributedCacheComponent;

    @Value("${folib.uploadRestrictions:false}")
    private boolean artifactUploadRestrictions;

    @Autowired
    private ThreadPoolTaskExecutor asyncCheckSumTaskExecutor;

    public long validateAndStore(RepositoryPath repositoryPath,
                                 InputStream is)
            throws IOException,
            ProviderImplementationException,
            ArtifactCoordinatesValidationException {
        repositoryPath = performRepositoryAcceptanceValidation(repositoryPath);
        return doStore(repositoryPath, is);
    }

    public long validateAndStore(RepositoryPath repositoryPath,
                                 Path sourcePath)
            throws IOException,
            ProviderImplementationException,
            ArtifactCoordinatesValidationException {
        repositoryPath = performRepositoryAcceptanceValidation(repositoryPath);
        return doStore(repositoryPath, sourcePath);
    }

    public void validateAndStoreIndex(RepositoryPath repositoryPath)
            throws IOException,
            ProviderImplementationException,
            ArtifactCoordinatesValidationException {
        repositoryPath = performStoreIndexRepositoryAcceptanceValidation(repositoryPath);
        doStoreIndex(repositoryPath);
    }

    public long store(RepositoryPath repositoryPath,
                      InputStream is)
            throws IOException {
        repositoryPath = performStoreRepositoryAcceptanceValidation(repositoryPath);
        return doStore(repositoryPath, is);
    }

    public long store(RepositoryPath repositoryPath,
                      RepositoryPath sourcePath)
            throws IOException {
        repositoryPath = performStoreRepositoryAcceptanceValidation(repositoryPath);
        return doStore(repositoryPath, sourcePath);
    }

    private long doStore(RepositoryPath repositoryPath,
                         InputStream is)
            throws IOException {
        validateRepositoryPath(repositoryPath, is, null);
        StopWatch stopWatch = new StopWatch("Store step");
        stopWatch.start("doStore");
        long result;
        try (final RepositoryStreamSupport.RepositoryOutputStream aos = artifactResolutionService.getOutputStream(repositoryPath)) {
            result = writeArtifact(repositoryPath, is, aos);
            logger.info("Stored [{}] bytes for [{}].", result, repositoryPath);
            aos.flush();
        } catch (IOException e) {
            throw e;
        } catch (Exception e) {
            throw new ArtifactStorageException(e);
        } finally {
            // Check size
            if (artifactUploadRestrictions) {
                long fileSize = Files.size(repositoryPath);
                if (fileSize == 0) {
                    throw new ArtifactResolutionException("Uploaded file is empty.");
                }
                Repository repository = getConfiguration().getStorage(repositoryPath.getStorageId()).getRepository(repositoryPath.getRepositoryId());
                long artifactMaxSize = repository.getArtifactMaxSize();

                if (artifactMaxSize > 0 && fileSize > artifactMaxSize) {
                    delete(repositoryPath, true);
                    throw new ArtifactResolutionException("The size of the artifact exceeds the maximum size accepted by " +
                            "this repository (" + fileSize + "/" +
                            artifactMaxSize + ").");
                }
            }
        }
        stopWatch.stop();
        logger.info("【Store】finished [{}] \n {} .", repositoryPath,stopWatch.prettyPrint(TimeUnit.MILLISECONDS));
        return result;
    }

    private long doStore(RepositoryPath repositoryPath,
                         Path sourcePath)
            throws IOException {
        validateRepositoryPath(repositoryPath, null, sourcePath);
        long startTime = System.currentTimeMillis();
        long result;
        try (final RepositoryStreamSupport.RepositoryOutputStream aos = artifactResolutionService.getOutputStream(repositoryPath)) {
            result = writeArtifact(repositoryPath, sourcePath, aos);
            logger.debug("Stored [{}] bytes for [{}].", result, repositoryPath);
            aos.flush();
        } catch (IOException e) {
            throw e;
        } catch (Exception e) {
            throw new ArtifactStorageException(e);
        }
        logger.debug("DoStore [{}] take time [{}] ms", repositoryPath.toString(), System.currentTimeMillis() - startTime);

        return result;
    }

    private void doStoreIndex(RepositoryPath repositoryPath)
            throws IOException {
        try (final RepositoryStreamSupport.RepositoryStoreIndexInputStream ins = artifactResolutionService.getStoreIndexInputStream(repositoryPath)) {
            writeArtifactIndex(repositoryPath, ins);
            logger.debug("Stored index for [{}].", repositoryPath);
            ins.commitStoreIndex();
        } catch (IOException e) {
            throw e;
        } catch (Exception e) {
            throw new ArtifactStorageException(e);
        }
    }

    private long writeArtifact(RepositoryPath repositoryPath,
                               InputStream is,
                               OutputStream os)
            throws IOException {
        StopWatch stopWatch = new StopWatch("Write step");
        stopWatch.start("Read attribute");
        LayoutOutputStream aos = StreamUtils.findSource(LayoutOutputStream.class, os);
        Repository repository = repositoryPath.getRepository();
        Boolean checksumAttribute = RepositoryFiles.isChecksum(repositoryPath);
        stopWatch.stop();
        // If we have no digests, then we have a checksum to store.
        if (Boolean.TRUE.equals(checksumAttribute)) {
            aos.setCacheOutputStream(new ByteArrayOutputStream());
        }
        if (repository.isHostedRepository()) {
            artifactEventListenerRegistry.dispatchArtifactUploadingEvent(repositoryPath);
        }
        stopWatch.start("IOUtils copy");
        long totalAmountOfBytes = IOUtils.copy(is, os);
        stopWatch.stop();
        logger.info("【writeArtifact】 step stats \n{}", stopWatch.prettyPrint(TimeUnit.MILLISECONDS));

        URI repositoryPathId = repositoryPath.toUri();

        Map<String, String> digestMap = aos.getDigestMap(repository.getLayout());

        if (Boolean.FALSE.equals(checksumAttribute) && !digestMap.isEmpty()) {
            // Store artifact digests in cache if we have them.
            addChecksumsToCacheManager(digestMap, repositoryPathId);
            writeChecksums(repositoryPath, digestMap);
        }

        if (Boolean.TRUE.equals(checksumAttribute)) {
            byte[] checksumValue = ((ByteArrayOutputStream) aos.getCacheOutputStream()).toByteArray();
            if (checksumValue != null && checksumValue.length > 0) {
                // Validate checksum with artifact digest cache.
                validateUploadedChecksumAgainstCache(checksumValue, repositoryPathId);
            }
        }

        return totalAmountOfBytes;
    }

    private long writeArtifact(RepositoryPath repositoryPath,
                               Path sourcePath,
                               OutputStream os)
            throws IOException {
        LayoutOutputStream aos = StreamUtils.findSource(LayoutOutputStream.class, os);

        Repository repository = repositoryPath.getRepository();

        Boolean checksumAttribute = RepositoryFiles.isChecksum(repositoryPath);

        // If we have no digests, then we have a checksum to store.
        if (Boolean.TRUE.equals(checksumAttribute)) {
            aos.setCacheOutputStream(new ByteArrayOutputStream());
        }

        if (repository.isHostedRepository()) {
            artifactEventListenerRegistry.dispatchArtifactUploadingEvent(repositoryPath);
        }

        long startTime = System.currentTimeMillis();
        long totalAmountOfBytes = Files.copy(sourcePath, os);
        logger.debug("Files copy [{}] take time [{}] ms", repositoryPath.toString(), System.currentTimeMillis() - startTime);

        URI repositoryPathId = repositoryPath.toUri();
        Map<String, String> digestMap = aos.getDigestMap(repository.getLayout());
        if (Boolean.FALSE.equals(checksumAttribute) && !digestMap.isEmpty()) {
            // Store artifact digests in cache if we have them.
            addChecksumsToCacheManager(digestMap, repositoryPathId);
            writeChecksums(repositoryPath, digestMap);
        }

        if (Boolean.TRUE.equals(checksumAttribute)) {
            byte[] checksumValue = ((ByteArrayOutputStream) aos.getCacheOutputStream()).toByteArray();
            if (checksumValue != null && checksumValue.length > 0) {
                // Validate checksum with artifact digest cache.
                validateUploadedChecksumAgainstCache(checksumValue, repositoryPathId);
            }
        }

        return totalAmountOfBytes;
    }

    private void writeArtifactIndex(RepositoryPath repositoryPath,
                                    InputStream is)
            throws IOException {
        LayoutInputStream ins = StreamUtils.findSource(LayoutInputStream.class, is);
        if (Objects.isNull(ins)) {
            throw new IOException("repositoryPath LayoutInputStream not exists");
        }
        byte[] bytes = new byte[8192];
        while (ins.read(bytes) != -1) {

        }
        Repository repository = repositoryPath.getRepository();

        Boolean checksumAttribute = RepositoryFiles.isChecksum(repositoryPath);

        if (repository.isHostedRepository()) {
            artifactEventListenerRegistry.dispatchArtifactUploadingEvent(repositoryPath);
        }

        URI repositoryPathId = repositoryPath.toUri();
        Set<String> digestAlgorithmSet = repositoryPath.getFileSystem().getDigestAlgorithmSet();
        digestAlgorithmSet.forEach(item -> ins.getMessageDigestAsHexadecimalString(item, repository.getLayout()));
        Map<String, String> digestMap = ins.getDigestMap();
        if (Boolean.FALSE.equals(checksumAttribute) && !digestMap.isEmpty()) {
            // Store artifact digests in cache if we have them.
            addChecksumsToCacheManager(digestMap, repositoryPathId);
            writeChecksums(repositoryPath, digestMap);
        }
    }

    private void writeChecksums(RepositoryPath repositoryPath,
                                Map<String, String> digestMap) {
        LayoutFileSystemProvider provider = repositoryPath.getFileSystem().provider();
        asyncCheckSumTaskExecutor.execute(() -> {
            StopWatch stopWatch = new StopWatch("checksum step" );
            digestMap.entrySet()
                    .stream()
                    .forEach(entry -> {
                        stopWatch.start(entry.getKey()+":"+repositoryPath.toString());
                        final RepositoryPath checksumPath = provider.getChecksumPath(repositoryPath, entry.getKey());
                        try {
                            Files.write(checksumPath, entry.getValue().getBytes(StandardCharsets.UTF_8));
                            stopWatch.stop();
                        } catch (IOException ex) {
                            logger.error(ex.getMessage(), ex);
                        }
                    });
            logger.info("checksum write  finished. the stats is \n{}", stopWatch.prettyPrint(TimeUnit.MILLISECONDS));
        });


    }

    public void checksums(RepositoryPath repositoryPath, Map<String, String> digestMap) {
        if (Objects.nonNull(digestMap) && !digestMap.isEmpty()) {
            addChecksumsToCacheManager(digestMap, repositoryPath.toUri());
            writeChecksums(repositoryPath, digestMap);


        }
    }

    private void validateUploadedChecksumAgainstCache(byte[] checksum,
                                                      URI artifactPathId) {
        logger.debug("Received checksum: {}", new String(checksum, StandardCharsets.UTF_8));

        String artifactPath = artifactPathId.toString();
        String artifactBasePath = artifactPath.substring(0, artifactPath.lastIndexOf('.'));
        String checksumExtension = artifactPath.substring(artifactPath.lastIndexOf('.') + 1, artifactPath.length());

        if (!matchesChecksum(checksum, artifactBasePath, checksumExtension)) {
            logger.warn("The checksum for {} [{}] is invalid!",
                    artifactPath,
                    new String(checksum, StandardCharsets.UTF_8));
        }

        checksumCacheManager.removeArtifactChecksum(artifactBasePath, checksumExtension);
    }

    private boolean matchesChecksum(byte[] pChecksum,
                                    String artifactBasePath,
                                    String checksumExtension) {
        String checksum = new String(pChecksum, StandardCharsets.UTF_8);
        ArtifactChecksum artifactChecksum = checksumCacheManager.getArtifactChecksum(artifactBasePath);

        if (artifactChecksum == null) {
            return false;
        }

        Map<Boolean, Set<String>> matchingMap = artifactChecksum.getChecksums()
                .entrySet()
                .stream()
                .collect(Collectors.groupingBy(e -> e.getValue()
                                .equals(checksum),
                        Collectors.mapping(
                                e -> e.getKey(),
                                Collectors.toSet())));

        Set<String> matched = matchingMap.get(Boolean.TRUE);
        Set<String> unmatched = matchingMap.get(Boolean.FALSE);

        logger.debug("Artifact checksum matchings: artifact-[{}]; ext-[{}]; matched-[{}];" +
                        " unmatched-[{}]; checksum-[{}]",
                artifactBasePath,
                checksumExtension,
                matched,
                unmatched,
                checksum);

        return matched != null && !matched.isEmpty();
    }

//    private void asyncCheckSumToCacheAndWrite(Map<String, String> digestMap,
//                                              URI artifactPath,RepositoryPath repositoryPath) {
//
//        StopWatch stopWatch = new StopWatch("checksum step" );
//        LayoutFileSystemProvider provider = repositoryPath.getFileSystem().provider();
//        asyncCheckSumTaskExecutor.execute(() -> {
//            for (Map.Entry<String, String> entry : digestMap.entrySet()) {
//                checksumCacheManager.addArtifactChecksum(artifactPath.toString(), entry.getKey(), entry.getValue());
//                stopWatch.start(entry.getKey()+":"+repositoryPath.toString());
//                final RepositoryPath checksumPath = provider.getChecksumPath(repositoryPath, entry.getKey());
//                try {
//                    Files.write(checksumPath, entry.getValue().getBytes(StandardCharsets.UTF_8));
//                    stopWatch.stop();
//                } catch (IOException ex) {
//                    logger.error(ex.getMessage(), ex);
//                }
//            }
//            logger.info("checksum write  finished. the stats is \n{}", stopWatch.prettyPrint(TimeUnit.MILLISECONDS));
//        });
//    }

    private void addChecksumsToCacheManager(Map<String, String> digestMap,
                                            URI artifactPath) {
        digestMap.entrySet()
                .stream()
                .forEach(e -> checksumCacheManager.addArtifactChecksum(artifactPath.toString(), e.getKey(), e.getValue()));
    }

    public RepositoryPath performRepositoryAcceptanceValidation(RepositoryPath path)
            throws IOException, ProviderImplementationException, ArtifactCoordinatesValidationException {
        long startTime = System.currentTimeMillis();
        logger.debug("Validate artifact with path [{}]", path);

        path = validateRepositoryPathPrivilege(path);

        Repository repository = path.getFileSystem().getRepository();

        long validateStartTime = System.currentTimeMillis();
        artifactOperationsValidator.validate(path);
        logger.info("Repository artifactOperationsValidator [{}] take time [{}] ms.", path.toString(), System.currentTimeMillis() - validateStartTime);

        if (!RepositoryFiles.isArtifact(path)) {
            return path;
        }

        long readCoordinatesStartTime = System.currentTimeMillis();
        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(path);
        logger.info("Repository readCoordinates [{}] take time [{}] ms.", path.toString(), System.currentTimeMillis() - readCoordinatesStartTime);
        logger.debug("Validate artifact with coordinates [{}]", coordinates);

        try {
            long artifactCoordinatesValidatorStartTime = System.currentTimeMillis();
            for (String validatorKey : repository.getArtifactCoordinateValidators()) {
                ArtifactCoordinatesValidator validator = artifactCoordinatesValidatorRegistry.getProvider(
                        validatorKey);
                if (validator.supports(repository)) {
                    validator.validate(repository, coordinates);
                }
            }
            logger.info("Repository artifactCoordinatesValidator [{}] take time [{}] ms.", path.toString(), System.currentTimeMillis() - artifactCoordinatesValidatorStartTime);
        } catch (VersionValidationException e) {
            throw new ArtifactStorageException(e);
        }
        long checkAllowsStartTime = System.currentTimeMillis();
        artifactOperationsValidator.checkAllowsRedeployment(repository, coordinates);
        artifactOperationsValidator.checkAllowsDeployment(repository);
        logger.info("Repository checkAllows [{}] take time [{}] ms.", path.toString(), System.currentTimeMillis() - checkAllowsStartTime);
        if (RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            long checkStorageSizeStartTime = System.currentTimeMillis();
            logger.info("Repository checkStorageSize [{}] take time [{}] ms.", path.toString(), System.currentTimeMillis() - checkStorageSizeStartTime);
        }
        logger.info("Repository acceptance validation [{}] take time [{}] ms.", path.toString(), System.currentTimeMillis() - startTime);
        return path;
    }


    public RepositoryPath performStoreRepositoryAcceptanceValidation(RepositoryPath path)
            throws IOException {
        logger.debug("Validate artifact with path [{}]", path);

        path = validateRepositoryPathPrivilege(path);

        Repository repository = path.getFileSystem().getRepository();

        if (RepositoryTypeEnum.HOSTED.getType().equalsIgnoreCase(repository.getType())) {
            artifactOperationsValidator.validate(path);
        }
        if (!RepositoryFiles.isArtifact(path)) {
            return path;
        }

        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(path);
        logger.debug("Validate artifact with coordinates [{}]", coordinates);

        try {
            for (String validatorKey : repository.getArtifactCoordinateValidators()) {
                ArtifactCoordinatesValidator validator = artifactCoordinatesValidatorRegistry.getProvider(
                        validatorKey);
                if (validator.supports(repository)) {
                    if ((RedeploymentValidator.ALIAS.equalsIgnoreCase(validatorKey)) && RepositoryTypeEnum.PROXY.getType().equalsIgnoreCase(repository.getType())) {
                        continue;
                    }
                    validator.validate(repository, coordinates);
                }
            }
        } catch (Exception e) {
            throw new ArtifactStorageException(e);
        }
        return path;
    }

    private RepositoryPath performStoreIndexRepositoryAcceptanceValidation(RepositoryPath path)
            throws IOException, ProviderImplementationException, ArtifactCoordinatesValidationException {
        logger.debug("Validate artifact with path [{}]", path);

        path = validateRepositoryPathPrivilege(path);

        Repository repository = path.getFileSystem().getRepository();

        artifactOperationsValidator.validate(path);

        if (!RepositoryFiles.isArtifact(path)) {
            return path;
        }

        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(path);
        logger.debug("Validate artifact with coordinates [{}]", coordinates);

        try {
            for (String validatorKey : repository.getArtifactCoordinateValidators()) {
                if (RedeploymentValidator.ALIAS.equals(validatorKey)) {
                    continue;
                }
                ArtifactCoordinatesValidator validator = artifactCoordinatesValidatorRegistry.getProvider(
                        validatorKey);
                if (validator.supports(repository)) {
                    validator.validate(repository, coordinates);
                }
            }
        } catch (VersionValidationException e) {
            throw new ArtifactStorageException(e);
        }
        return path;
    }

    protected Storage getStorage(String storageId) {
        return getConfiguration().getStorages().get(storageId);
    }

    protected Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

    @Transactional(rollbackFor = Exception.class)
    public void delete(RepositoryPath repositoryPath,
                       boolean force)
            throws IOException {
        artifactOperationsValidator.validate(repositoryPath);

        final Repository repository = repositoryPath.getRepository();

        artifactOperationsValidator.checkAllowsDeletion(repository);

        try {
            RepositoryFiles.delete(repositoryPath, force);
        } catch (IOException e) {
            throw new ArtifactStorageException(e.getMessage(), e);
        }
    }

    public void copy(RepositoryPath srcPath, RepositoryPath destPath)
            throws IOException {
        artifactOperationsValidator.validate(srcPath);

        if (Files.isDirectory(srcPath)) {
            FileSystemUtils.copyRecursively(srcPath, destPath);
        } else {
            Files.copy(srcPath, destPath);
        }
    }

    public RepositoryPath validateRepositoryPathPrivilege(RepositoryPath repositoryPath) throws IOException {
        Repository repository = repositoryPath.getFileSystem().getRepository();
        if (Objects.isNull(repository)) {
            return repositoryPath;
        }
        if (repository.isGroupRepository() && StringUtils.isNotBlank(repository.getGroupDefaultRepository())) {
            if (repository.getLayout().equals(ProductTypeEnum.Cargo.getFoLibraryName()) && repositoryPath.toString().endsWith("config.json")) {
                return repositoryPath;
            }
            //是组合库，并且设置了默认上传仓库
            String storageId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), repository.getGroupDefaultRepository());
            String repositoryId = ConfigurationUtils.getRepositoryId(repository.getGroupDefaultRepository());
            if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                Storage storage = configurationManager.getStorage(storageId);
                if (Objects.isNull(storage)) {
                    return repositoryPath;
                }
                Repository storageRepository = storage.getRepository(repositoryId);
                if (Objects.isNull(storageRepository)) {
                    return repositoryPath;
                }
                RepositoryPath subRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, RepositoryFiles.relativizePath(repositoryPath));
                boolean result = artifactSecurityComponent.validatePrivileges(subRepositoryPath, Privileges.ARTIFACTS_DEPLOY.getAuthority());
                if (result) {
                    repositoryPath = subRepositoryPath;
                } else {
                    throw new ArtifactStorageException(String.format("No permission to deploy repositoryPath [%s]", repositoryPath));
                }
            }
        } else if (!repository.isGroupRepository()) {
            if (!artifactSecurityComponent.anonymousValidatePrivilege(repositoryPath)) {
                return repositoryPath;
            }
            boolean result = artifactSecurityComponent.validatePrivileges(repositoryPath, Privileges.ARTIFACTS_DEPLOY.getAuthority());
            if (!result) {
                throw new ArtifactStorageException(String.format("No permission to deploy repositoryPath [%s]", repositoryPath));
            }
        }
        return repositoryPath;
    }

    private boolean enableValidateInputStreamEmpty() {
        String value = distributedCacheComponent.get("ENABLE_VALIDATE_INPUT_STREAM_EMPTY");
        if (StringUtils.isNotBlank(value)) {
            return Boolean.parseBoolean(value);
        }
        return false;
    }

    private boolean validateInputStreamEmpty(RepositoryPath repositoryPath, InputStream inputStream) {
        try {
            if (!enableValidateInputStreamEmpty()) {
                return true;
            }
            return Objects.nonNull(inputStream) && inputStream.available() != 0;
        } catch (Exception ex) {
            logger.error("Validate inputStream empty [{}] error [{}]", repositoryPath, ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }

    private boolean validateRepositoryPathEmpty(RepositoryPath repositoryPath, Path sourcePath) {
        try {
            return Files.exists(sourcePath) && Files.size(sourcePath) > 0;
        } catch (Exception ex) {
            logger.error("Validate inputStream empty [{}] error [{}]", repositoryPath, ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }

    private void validateRepositoryPath(RepositoryPath repositoryPath, InputStream inputStream, Path sourcePath) throws IOException {
        if (Objects.nonNull(inputStream) && !validateInputStreamEmpty(repositoryPath, inputStream)) {
            throw new ArtifactResolutionException("The repositoryPath content is empty.");
        }
        if (Objects.nonNull(sourcePath) && !validateRepositoryPathEmpty(repositoryPath, sourcePath)) {
            throw new ArtifactResolutionException("The repositoryPath content is empty.");
        }
        String path = RepositoryFiles.relativizeOriginalPath(repositoryPath.getParent());
        if (!DirectoryValidatorUtils.validateDirectoryPath(path)) {
            throw new ArtifactResolutionException(String.format("Path [%s] contains illegal characters", path));
        }
    }

}
