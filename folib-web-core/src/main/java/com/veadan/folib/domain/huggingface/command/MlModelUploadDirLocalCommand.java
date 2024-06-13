package com.veadan.folib.domain.huggingface.command;


import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.google.common.annotations.VisibleForTesting;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import javax.annotation.Nullable;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.huggingface.common.ConflictsGuard;
import com.veadan.folib.domain.huggingface.index.MlModelIndexHandler;
import com.veadan.folib.domain.huggingface.model.request.MlKeyValue;
import com.veadan.folib.domain.huggingface.model.request.MlModelRequestContext;
import com.veadan.folib.domain.huggingface.utils.MlModelUtils;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.HuggingFaceLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import lombok.Generated;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MlModelUploadDirLocalCommand {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(MlModelUploadDirLocalCommand.class);

    private static final String CONTENT_FIELD = "content";

    private static final String VALUE_FIELD = "value";

    private static final String KEY_FIELD = "key";

    private static final String PATH_FIELD = "path";

    private static final String ENCODING_FIELD = "encoding";

    private static final String BASE_64_ENCODING = "base64";

    private static final String HEADER_OBJECT = "header";

    private static final String FILE_OBJECT = "file";

    private static final String LFS_FILE_OBJECT = "lfsFile";

    private static final String OID = "oid";

    private static final String SUMMARY = "summary";

    private static final String Temp_UploadDir = "hfml_upload";

    private static final JsonFactory JSON_FACTORY = new JsonFactory();


    private final MlModelPreUploadDirLocalCommand preUploadDirLocalCommand;
    private RepositoryPathResolver repositoryPathResolver;
    private ArtifactManagementService artifactManagementService;
    private HuggingFaceLayoutProvider layoutProvide;
    private MlModelIndexHandler indexHandler;
    private ArtifactRepository artifactRepository;


    public MlModelUploadDirLocalCommand(RepositoryPathResolver repositoryPathResolver,
                                        ArtifactManagementService artifactManagementService,
                                        HuggingFaceLayoutProvider layoutProvide,
                                        ArtifactRepository artifactRepository) {
        this.preUploadDirLocalCommand = new MlModelPreUploadDirLocalCommand();
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.layoutProvide = layoutProvide;
        this.indexHandler = new MlModelIndexHandler(repositoryPathResolver, artifactManagementService, layoutProvide);
        this.artifactRepository = artifactRepository;
    }

    public String uploadDir(MlModelRequestContext requestContext, InputStream bodyStream) {
        String commitSummary;
        if (requestContext == null) {
            throw new NullPointerException("requestContext is marked non-null but is null");
        }
        if (bodyStream == null) {
            throw new NullPointerException("bodyStream is marked non-null but is null");
        }
        String repoKey = requestContext.getRepositoryId();
        String organization = requestContext.getOrg();
        String modelName = requestContext.getModelName();
        String revision = requestContext.getRevision();
        String subRevisionFolder = MlModelUtils.formattedDate();
        log.debug("Received commit request for repo {}, organization {}, model {}, revision {}", repoKey, organization, modelName, revision);
        this.preUploadDirLocalCommand.assertValidNames(requestContext);
        this.preUploadDirLocalCommand.assertModuleAlreadyExist(requestContext);
        Set<String> uploadedFiles = new HashSet<>();
        MlKeyValue commitInfo = uploadStream(bodyStream, uploadedFiles, requestContext.getStorageId(), requestContext.getRepositoryId(), organization, modelName, revision, subRevisionFolder);
        //TODO: 2024/6/6
        // ConflictsGuard<String> handleCommitGuard =this.conflictsGuardService.getConflictsGuard("handle_commit_guard");
        String lockName = String.join("/", repoKey, MlModelUtils.getLatestLeadFilePath(organization, modelName, revision));
        //try {
        //handleCommitGuard.tryToLock(lockName, 30L, TimeUnit.SECONDS);
        // TODO: 2024/6/6
        this.indexHandler.processUploadComplete(requestContext, subRevisionFolder);
        //} catch (InterruptedException e) {
        //    log.error("Failed to acquire lock for processUploadComplete: '{}'. {}'", lockName, e.getMessage());
        //    Thread.currentThread().interrupt();
        //} finally {
        //    handleCommitGuard.unlock(lockName);
        //}
        if (commitInfo != null && commitInfo.getValue() != null) {
            commitSummary = (String) commitInfo.getValue().get("summary");
        } else {
            log.warn("CommitSummary or its value is null for requestContext {}", requestContext);
            commitSummary = "";
        }
        return commitSummary;
    }

    public List<MlKeyValue> extractParamsFromJson(InputStream jsonInputStream, Path tmpUploadDir) throws IOException {
        if (jsonInputStream == null) {
            throw new NullPointerException("jsonInputStream is marked non-null but is null");
        }
        if (tmpUploadDir == null) {
            throw new NullPointerException("tmpUploadDir is marked non-null but is null");
        }
        List<MlKeyValue> genericFilesData = new ArrayList<>();
        try {
            JsonParser jsonParser = JSON_FACTORY.createParser(jsonInputStream);
            try {
                while (jsonParser.nextToken() != null) {
                    if (jsonParser.getCurrentToken() == JsonToken.START_OBJECT) {
                        genericFilesData.add(parseKeyValue(tmpUploadDir, jsonParser));
                    }
                }
                if (jsonParser != null) {
                    jsonParser.close();
                }
            } catch (Throwable throwable) {
                if (jsonParser != null) {
                    try {
                        jsonParser.close();
                    } catch (Throwable throwable1) {
                        throwable.addSuppressed(throwable1);
                    }
                }
                throw throwable;
            }
        } catch (Exception e) {
            for (MlKeyValue uploadInfo : genericFilesData) {
                if ("header".equals(uploadInfo.getKey())) {
                    continue;
                }
                String pathOnFS =  uploadInfo.getValue().get("content");
                deleteTempFileFromFS(pathOnFS);
            }
            throw e;
        }
        return genericFilesData;
    }


    private MlKeyValue parseKeyValue(Path tmpUploadDir, JsonParser jsonParser) throws IOException {
        if (tmpUploadDir == null) {
            throw new NullPointerException("tmpUploadDir is marked non-null but is null");
        }
        MlKeyValue mlKeyValue = new MlKeyValue();
        while (jsonParser.nextToken() != JsonToken.END_OBJECT) {
            String field = jsonParser.getCurrentName();
            jsonParser.nextToken();
            if ("key".equals(field)) {
                mlKeyValue.setKey(jsonParser.getValueAsString());
                continue;
            }
            if (field.equals("value")) {
                mlKeyValue.setValue(readValueAsMap(jsonParser, tmpUploadDir));
            }
        }
        return mlKeyValue;
    }

    @VisibleForTesting
    public void deleteTempFileFromFS(String pathOnFS) throws IOException {
        Files.deleteIfExists(Paths.get(pathOnFS, new String[0]));
    }

    private MlKeyValue uploadStream(InputStream bodyStream, Set<String> uploadedFiles, String storageId, String repositoryId, String organization, String modelName, String revision, String subRevisionFolder) {
        try {
            // 创建一个临时目录
            Path tempDir = Files.createTempDirectory(Temp_UploadDir);
            List<MlKeyValue> mlKeyValues = extractParamsFromJson(bodyStream, tempDir);
            return uploadEntries(mlKeyValues, storageId, repositoryId, organization, modelName, revision, subRevisionFolder, uploadedFiles);
        } catch (IOException e) {
            log.warn("Failed to parse commit request body for repoKey {} organization {} modelName: {}, revision: {}. Message {}", repositoryId, organization, modelName, revision, e.getMessage());
            log.debug("Failed to parse commit request body for repoKey {} organization {} modelName: {}, revision: {}", repositoryId, organization, modelName, revision, e);
            throw new RuntimeException("Failed to parse commit request body", e);
        }
    }

    @Nullable
    private MlKeyValue uploadEntries(List<MlKeyValue> entriesToUpload, String storageId, String repositoryId, String organization, String modelName, String revision, String subRevision, Set<String> uploadedFiles) throws IOException {
        MlKeyValue commitInfo = null;
        for (MlKeyValue uploadInfo : entriesToUpload) {
            if (uploadInfo.getValue() == null || uploadInfo.getValue().isEmpty()) {
                log.warn("Received commit request with an empty value for repoKey {} organization {} modelName: {}, revision: {}", repositoryId, organization, modelName, revision);
                throw new RuntimeException("Received commit request with empty upload value");
            }
            switch (uploadInfo.getKey()) {
                case "header":
                    commitInfo = uploadInfo;
                    continue;
                case "file":
                    processGenericFileUpload(storageId, repositoryId, organization, modelName, revision, subRevision, uploadedFiles, uploadInfo);
                    continue;
                case "lfsFile":
                    processLfsFileUpload(storageId, repositoryId, organization, modelName, revision, subRevision, uploadedFiles, uploadInfo);
                    continue;
            }
            log.warn("Received commit request with unsupported key: {} for repoKey {} organization {} modelName: {}, revision: {}", uploadInfo.getKey(), repositoryId, organization, modelName, revision);
        }
        return commitInfo;
    }

    private void processLfsFileUpload(String storageId, String repositoryId, String organization, String modelName, String revision, String subRevision, Set<String> uploadedFiles, MlKeyValue uploadInfo) {
        String oid = uploadInfo.getValue().get("oid");
        String fileName = uploadInfo.getValue().get("path");
        String path = MlModelUtils.getFilePath(organization, modelName, revision, subRevision, fileName);
        uploadedFiles.add(path);

        Artifact artifact= artifactRepository.findOneArtifact(storageId, repositoryId, path);
        //Stream<PackageArtifact> oidFiles = this.searchService.findArtifactsChildrenWithName(repoKey,
        //        MlModelUtils.getLfsTmpUploadDir(organization, modelName), oid + "*");
        //Optional<PackageArtifact> any = oidFiles.findAny();
        if (artifact !=null) {
            log.warn("No content for oid {} found for repo {}, organization {}, modelName {}, revision {}", oid, repositoryId, organization, modelName, revision);
        } else {
            String sourcePath =  MlModelUtils.getLfsTmpUploadPath(organization, modelName, oid);
            String destinationPath = MlModelUtils.getFilePath(organization, modelName, revision, subRevision, fileName);
            log.debug("Copying file with oid {} for repo {}, organization {}, modelName {}, revision {}", oid, repositoryId, organization, modelName, revision);
            //this.repositoryService.copy(repoKey, repoKey, sourcePath, destinationPath);
            RepositoryPath srcPath =  repositoryPathResolver.resolve(storageId, repositoryId, sourcePath);
            RepositoryPath destPath = repositoryPathResolver.resolve(storageId, repositoryId, destinationPath);
            try {
                artifactManagementService.validateAndStore(destPath, new BufferedInputStream(Files.newInputStream(srcPath)));
            } catch (IOException |  ProviderImplementationException | ArtifactCoordinatesValidationException e) {
                log.error("Failed to copy file with oid {} for repo {}, organization {}, modelName {}, revision {}", oid, repositoryId, organization, modelName, revision);
                throw new RuntimeException(e);
            }
        }

    }

    private void processGenericFileUpload(String storageId, String repositoryId, String organization, String modelName, String revision, String subRevision, Set<String> uploadedFiles, MlKeyValue uploadInfo) throws IOException {
        String path = MlModelUtils.getFilePath(organization, modelName, revision, subRevision, uploadInfo
                .getValue().get("path"));
        log.debug("Processing file {} for repoKey {}, organization {}, modelName {}, revision {}", path, repositoryId, organization, modelName, revision);
        String encoding = uploadInfo.getValue().get("encoding");
        if (!"base64".equals(encoding)) {
            log.warn("Received commit request with unsupported encoding: {} for repoKey {} organization {} modelName: {}, revision: {}", encoding, repositoryId, organization, modelName, revision);
            throw new RuntimeException("Unsupported encoding: " + encoding);
        }
        uploadedFiles.add(path);
        String contentPath = uploadInfo.getValue().get("content");
        try {
            if (skipUploading(storageId,repositoryId, path, contentPath)) {
                return;
            }
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            Path filePath = Paths.get(contentPath, new String[0]);
            artifactManagementService.validateAndStore(repositoryPath, new BufferedInputStream(Files.newInputStream(filePath)));
        } catch (IOException | ProviderImplementationException | ArtifactCoordinatesValidationException e) {
            log.error("upload file error", e);
        } finally {
            Path tmpUploadPath = Paths.get(contentPath, new String[0]);
            Files.deleteIfExists(tmpUploadPath);
        }
    }

    private boolean skipUploading(String storageId,String repositoryId, String path, String contentPath) {

        try {
            String sha2 = MlModelUtils.sha2(contentPath);
            Artifact artifact= artifactRepository.findOneArtifact(storageId, repositoryId, path);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            String sha23 = MlModelUtils.sha2(repositoryPath.getPath());
            if (artifact != null && sha2.equals(sha23)) {
                log.debug("Skipping upload of file {} since it already exists in repo {}", path, repositoryId);
                return true;
            }
            //PackageArtifact existingArtifact = this.downloadService.getArtifact(repositoryId, path);
            //if (existingArtifact != null && existingArtifact.getSha2().equals(sha2)) {
            //    log.debug("Skipping upload of file {} since it already exists in repo {}", path, repoKey);
            //    return true;
            //}
        } catch (Exception e) {
            log.debug("Artifact {} does not exist in repo {}", path, repositoryId);
        }
        return false;
    }

    private Map<String, String> readValueAsMap(JsonParser valueParser, Path tmpUploadDir) throws IOException {
        Map<String, String> map = new HashMap<>();
        if (valueParser.getCurrentToken() == JsonToken.START_OBJECT) {
            while (valueParser.nextToken() != JsonToken.END_OBJECT) {
                String key = valueParser.getCurrentName();
                valueParser.nextToken();
                if ("content".equals(key)) {
                    String filePath = String.join("/",tmpUploadDir.toAbsolutePath().toString(),"huggingface_" + UUID.randomUUID());
                    try {
                        FileOutputStream fos = new FileOutputStream(filePath);
                        try {
                            valueParser.readBinaryValue(fos);
                            map.put(key, filePath);
                            fos.close();
                        } catch (Throwable throwable) {
                            try {
                                fos.close();
                            } catch (Throwable throwable1) {
                                throwable.addSuppressed(throwable1);
                            }
                            throw throwable;
                        }
                    } catch (Exception e) {
                        log.warn("error during content parsing. Message {}", e.getMessage());
                        log.debug("error during content parsing.", e);
                    }
                    continue;
                }
                map.put(key, valueParser.getText());
            }
        }
        return map;
    }
}
