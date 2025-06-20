//package com.veadan.folib.index;
//
//
//import java.io.*;
//import java.nio.file.*;
//import java.nio.file.attribute.BasicFileAttributes;
//import java.util.*;
//import java.util.stream.Collectors;
//
//import com.fasterxml.jackson.databind.SerializationFeature;
//import com.google.common.collect.Multimap;
//import com.veadan.folib.command.MlModelFetchRevisionLocalCommand;
//import com.veadan.folib.model.CardData;
//import com.veadan.folib.model.RevisionData;
//import com.veadan.folib.model.SiblingItem;
//import com.veadan.folib.model.request.MlModelRequestContext;
//import com.veadan.folib.utils.MlModelUtils;
//import com.veadan.folib.providers.ProviderImplementationException;
//import com.veadan.folib.providers.io.RepositoryPath;
//import com.veadan.folib.providers.io.RepositoryPathResolver;
//import com.veadan.folib.providers.layout.HuggingFaceLayoutProvider;
//
//import com.veadan.folib.services.ArtifactManagementService;
//import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
//import lombok.Generated;
//import lombok.NonNull;
//import org.apache.commons.lang3.StringUtils;
//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;
//
//public class MlModelIndexHandler {
//    @Generated
//    private static final Logger log = LoggerFactory.getLogger(MlModelIndexHandler.class);
//
//    private static final String README_FILE_NAME_LOWER = "readme.md";
//
//    private final MlModelFetchRevisionLocalCommand fetchRevisionLocalCommand;
//
//    protected RepositoryPathResolver repositoryPathResolver;
//
//    protected ArtifactManagementService artifactManagementService;
//
//    private HuggingFaceLayoutProvider layoutProvider;
//
//    public MlModelIndexHandler(RepositoryPathResolver repositoryPathResolver, ArtifactManagementService artifactManagementService, HuggingFaceLayoutProvider layoutProvider) {
//        this.fetchRevisionLocalCommand = new MlModelFetchRevisionLocalCommand(repositoryPathResolver);
//        this.repositoryPathResolver = repositoryPathResolver;
//        this.artifactManagementService = artifactManagementService;
//        this.layoutProvider = layoutProvider;
//    }
//
//    public void processUploadComplete(MlModelRequestContext requestContext, String subRevisionFolder) {
//        if (requestContext == null) {
//            throw new NullPointerException("requestContext is marked non-null but is null");
//        }
//        String fullModelName = MlModelUtils.getModelId(requestContext.getOrg(), requestContext.getModelName());
//        log.debug("Starting MlModel index calculation repo: {}, model name is: {}", requestContext.getRepositoryId(), fullModelName);
//        try {
//            createdAndUploadLeadFile(requestContext, subRevisionFolder);
//        } catch (Exception e) {
//            log.error("Error while processing upload complete for repo: {}, model name is: {}", requestContext.getRepositoryId(), fullModelName, e);
//        }
//        //this.packageHandlerService.securityService()
//        //        .callAsSystem(() -> {
//        //            createdAndUploadLeadFile(requestContext, subRevisionFolder);
//        //            return null;
//        //        });
//        log.debug("MlModel index calculation ended successfully for repo: {}, model name is: {}", requestContext.getRepositoryId(), fullModelName);
//    }
//
//    private void createdAndUploadLeadFile(MlModelRequestContext requestContext, String subRevisionFolder) throws IOException, ProviderImplementationException, ArtifactCoordinatesValidationException {
//        String repositoryId = requestContext.getRepositoryId();
//        String organization = requestContext.getOrg();
//        String modelName = requestContext.getModelName();
//        String revision = requestContext.getRevision();
//        RevisionData dataToSerialize = getDataToSerialize(subRevisionFolder, requestContext.getStorageId(), repositoryId, organization, modelName, revision);
//        updateLeadFile(subRevisionFolder, requestContext.getStorageId(), repositoryId, organization, modelName, revision, dataToSerialize);
//        if (MlModelUtils.isReleaseRevision(requestContext)) {
//            RevisionData latestRevision;
//            try {
//                latestRevision = getFetchRevisionLocalCommand().fetchRevision(requestContext);
//            } catch (Exception packageException) {
//                log.debug("Latest lead file for model: {}, revision: {} doesn't exist. It can happen on the first upload", modelName, revision);
//                latestRevision = dataToSerialize;
//            }
//            String latestRevisionTimestamp = latestRevision.getLastModified();
//            if (latestRevisionTimestamp != null &&
//                    MlModelUtils.isIsoInstantFormat(latestRevisionTimestamp) && dataToSerialize
//                    .getLastModified() != null &&
//                    MlModelUtils.isIsoInstantFormat(dataToSerialize.getLastModified()) && !latestRevisionTimestamp.equals(subRevisionFolder)) {
//                Date deletionDate, latestRevisionDate = MlModelUtils.convertToDate(latestRevisionTimestamp);
//                Date uploadRevisionDate = MlModelUtils.convertToDate(dataToSerialize.getLastModified());
//                if (latestRevisionDate.before(uploadRevisionDate) || latestRevisionDate.equals(uploadRevisionDate)) {
//                    deletionDate = uploadRevisionDate;
//                } else {
//                    deletionDate = latestRevisionDate;
//                }
//                log.debug("Revision: {} is release version. going to calculate and delete old versions", requestContext.getRevision());
//                deleteOldReleaseBranchSubRevisions(requestContext, deletionDate);
//            }
//        }
//    }
//
//    private void deleteOldReleaseBranchSubRevisions(MlModelRequestContext requestContext, Date subRevisionFolder) {
//        if (requestContext == null) {
//            throw new NullPointerException("requestContext is marked non-null but is null");
//        }
//        if (subRevisionFolder == null) {
//            throw new NullPointerException("subRevisionFolder is marked non-null but is null");
//        }
//        String modelRevisionPath = MlModelUtils.getModelRevisionPath(requestContext);
//        String artifactRevisionTimestamp = MlModelUtils.extractSubRevisionFromPath(modelRevisionPath, StringUtils.isNotBlank(requestContext.getOrg()));
//        try {
//            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(requestContext.getStorageId(), requestContext.getRepositoryId(), modelRevisionPath);
//
//            if (Files.exists(repositoryPath)) {
//                artifactManagementService.delete(repositoryPath, false);
//                log.info("Deleted old subRevision {} for repoKey {} organization {} modelName: {}, revision: {}", artifactRevisionTimestamp, requestContext.getRepositoryId(), requestContext
//                        .getOrg(), requestContext.getModelName(), requestContext.getRevision());
//            }
//        } catch (IOException e) {
//            log.warn("Failed to parse subRevision {} for repoKey {} organization {} modelName: {}, revision: {}. Message {}", artifactRevisionTimestamp, requestContext.getRepositoryId(), requestContext.getOrg(), requestContext.getModelName(), requestContext.getRevision(), e.getMessage());
//            log.debug("Failed to parse subRevision {} for repoKey {} organization {} modelName: {}, revision: {}", artifactRevisionTimestamp, requestContext.getRepositoryId(), requestContext.getOrg(), requestContext.getModelName(), requestContext.getRevision(), e);
//        }
//    }
//
//    private void updateLeadFile(String subRevisionFolder, String storageId, String repositoryId, String organization, String modelName, String revision, RevisionData dataToSerialize) throws IOException, ProviderImplementationException, ArtifactCoordinatesValidationException {
//        String leadFilePath = MlModelUtils.getFilePath(organization, modelName, revision, subRevisionFolder, ".folib_huggingface_model_info.json");
//        //this.packageHandlerService.uploadService().upload(repositoryId, leadFilePath, new ByteArrayInputStream(
//        //        MlModelUtils.createObjectMapper().writeValueAsBytes(dataToSerialize)));
//        Multimap<String, String> attributes = MlModelUtils.extractAttributesFromRevisionData(dataToSerialize, repositoryId, organization, modelName, revision);
//        if (!attributes.isEmpty()) {
//            log.debug("Setting attributes {} for repo {} model {} revision {} organization {}", attributes, repositoryId, modelName, revision, organization);
//            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, leadFilePath);
//            artifactManagementService.validateAndStore(repositoryPath, new ByteArrayInputStream(MlModelUtils.createObjectMapper().enable(SerializationFeature.INDENT_OUTPUT).writeValueAsString(dataToSerialize).getBytes()));
//            //this.repositoryService.setAttributes(repoKey, leadFilePath, attributes);
//            //PackageArtifact artifact = this.downloadService.getArtifact(repoKey, leadFilePath);
//            //if (artifact == null) {
//            //    log.warn("Failed to retrieve artifact for repo {} model {} revision {} organization {}", repoKey, modelName, revision, organization);
//            //    return;
//            //}
//            //this.metadataServiceIndexer.indexPackageAsync(artifact, attributes);
//        }
//    }
//
//    public MlModelFetchRevisionLocalCommand getFetchRevisionLocalCommand() {
//        return this.fetchRevisionLocalCommand;
//    }
//
//    @NonNull
//    private RevisionData getDataToSerialize(String subRevisionFolder, String storageId, String repositoryId, String organization, String modelName, String revision) throws IOException {
//        RevisionData dataToSerialize, revisionData = new RevisionData();
//        revisionData.setCardData(new CardData());
//        List<String> readmePaths = new ArrayList<>();
//        revisionData.setSiblings(new ArrayList());
//        String requestedRevisionPath = MlModelUtils.getFilePath(organization, modelName, revision, subRevisionFolder, "");
//        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, requestedRevisionPath);
//        List<Path> fileList = new ArrayList<>();
//
//        try {
//            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<>() {
//                @Override
//                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
//                    // 在这里可以处理目录（如果需要的话）
//                    return FileVisitResult.CONTINUE;
//                }
//
//                @Override
//                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
//                    if (!file.getFileName().toString().startsWith(".")
//                            && !file.getFileName().toString().endsWith(".metadata")
//                            && !file.getFileName().toString().endsWith(".md5")
//                            && !file.getFileName().toString().endsWith(".sha1")
//                            && !file.getFileName().toString().endsWith(".sha256")) {
//                        fileList.add(file);
//                    }
//                    return FileVisitResult.CONTINUE;
//                }
//
//                @Override
//                public FileVisitResult visitFileFailed(Path file, IOException exc) {
//                    // 处理无法访问的文件
//                    log.error("访问文件失败: " + file.toString());
//                    exc.printStackTrace();
//                    return FileVisitResult.CONTINUE;
//                }
//            });
//
//        } catch (IOException e) {
//            log.error("访问文件失败: " + repositoryPath.toString());
//            e.printStackTrace();
//        }
//
//        fileList.forEach(artifact -> {
//            SiblingItem sibling = new SiblingItem();
//            if (artifact.getFileName().toString().equalsIgnoreCase("readme.md")) {
//                readmePaths.add(artifact.toAbsolutePath().toString());
//            }
//            sibling.setFileName(artifact.getFileName().toString().replace(requestedRevisionPath, ""));
//            revisionData.getSiblings().add(sibling);
//        });
//
//        if (!readmePaths.isEmpty()) {
//            if (readmePaths.size() > 1) {
//                log.warn("More than one readme file found for repoKey {}, organization {}, model {} revision {}. Only the first one will be used.", repositoryId, organization, modelName, revision);
//            }
//            String readmePath = readmePaths.get(0);
//            try {
//                //InputStream stream = this.downloadService.getStream(repositoryId, readmePath);
//                byte[] packageJsonBytes = layoutProvider.getContentByFileName(repositoryPath, repositoryPath, readmePath);
//                InputStream stream = new ByteArrayInputStream(packageJsonBytes);
//                try {
//                    dataToSerialize = MlModelIndexUtils.parseReadme(stream);
//                    if (StringUtils.isBlank(dataToSerialize.getModelId())) {
//                        dataToSerialize.setModelId(MlModelUtils.getModelId(organization, modelName));
//                    }
//                    dataToSerialize.setSiblings(revisionData.getSiblings());
//                    log.info("Parsed readme file for repo {} model {} revision {} organization {} license {}", repositoryId, dataToSerialize
//                            .getModelId(), revision, organization, dataToSerialize
//                            .getCardData().getLicense());
//                    if (stream != null) {
//                        stream.close();
//                    }
//                } catch (Throwable throwable) {
//                    if (stream != null) {
//                        try {
//                            stream.close();
//                        } catch (Throwable throwable1) {
//                            throwable.addSuppressed(throwable1);
//                        }
//                    }
//                    throw throwable;
//                }
//            } catch (Exception e) {
//                log.error("Failed to parse readme file for repo {} model {} revision {} organization {}", repositoryId, modelName, revision, organization, e);
//                dataToSerialize = revisionData;
//            }
//        } else {
//            log.warn("No readme file found for repoKey {}, organization {}, model {} revision {}", repositoryId, organization, modelName, revision);
//            dataToSerialize = revisionData;
//        }
//        dataToSerialize.setSha(MlModelUtils.getGeneratedCommitHash(revision, subRevisionFolder));
//        dataToSerialize.setLastModified(subRevisionFolder);
//        dataToSerialize.setTags(new LinkedList());
//        dataToSerialize.setId(MlModelUtils.getModelId(organization, modelName));
//        dataToSerialize.setPrivateProperty(false);
//        dataToSerialize.setSiblings(dataToSerialize
//                .getSiblings().stream().filter(sibling -> !sibling.getFileName().equalsIgnoreCase(".folib_huggingface_model_info.json")).collect(Collectors.toList()));
//        return dataToSerialize;
//    }
//}
//
