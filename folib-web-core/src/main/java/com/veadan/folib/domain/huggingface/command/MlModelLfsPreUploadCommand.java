package com.veadan.folib.domain.huggingface.command;


import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.gitls.command.LfsBaseUploadCommand;
import com.veadan.folib.domain.gitls.model.GitLfsBatchJson;
import com.veadan.folib.domain.gitls.model.GitLfsJson;
import com.veadan.folib.domain.huggingface.utils.MlModelUtils;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;

public class MlModelLfsPreUploadCommand extends LfsBaseUploadCommand {

    private static final Logger log = LoggerFactory.getLogger(MlModelLfsPreUploadCommand.class);

    static final long EXPIRATION_TIME_FOR_SIGNED_URL_SEC = 600L;

    protected RepositoryPathResolver repositoryPathResolver;

    private ArtifactManagementService artifactManagementService;

    public MlModelLfsPreUploadCommand(ArtifactRepository artifactRepository, ConfigurationManager configurationManager, RepositoryPathResolver repositoryPathResolver, ArtifactManagementService artifactManagementService) {
        super(artifactRepository, configurationManager);
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
    }

    public GitLfsBatchJson preUploadBatch(String storageId, String repositoryId, String organization, String modelName, GitLfsBatchJson batchLfsJson, HttpServletRequest request) {
        if (repositoryId == null) {
            throw new NullPointerException("repoKey is marked non-null but is null");
        }
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }
        if (batchLfsJson == null) {
            throw new NullPointerException("batchLfsJson is marked non-null but is null");
        }
        if (request == null) {
            throw new NullPointerException("request is marked non-null but is null");
        }
        log.debug("Received batch lfs pre-upload request for repo/organization/modelName '{}'/'{}'/'{}'", repositoryId, organization, modelName);
        List<GitLfsJson> responseJsons = new ArrayList<>();
        String lfsTmpUploadPath = MlModelUtils.getLfsTmpUploadDir(organization, modelName);
        //todo
        //if (!this.securityService.canWrite(repoKey, lfsTmpUploadPath)) {
        //    String errorMessage = "Forbidden: user is missing deploy permission on path: " + lfsTmpUploadPath;
        //    throw new PackageForbiddenException(errorMessage, errorMessage);
        //}

        String baserUrl = configurationManager.getConfiguration().getBaseUrl();
        baserUrl = baserUrl.endsWith("/") ? baserUrl.substring(0, baserUrl.length() - 1) + "/storages" : baserUrl + "/storages";
        for (GitLfsJson requestJson : batchLfsJson.getObjects()) {
            boolean sha2ReusePossible = tryToReuseExistingSha2(storageId, repositoryId, organization, modelName, requestJson);
            if (sha2ReusePossible) {
                log.debug("Same sha2 '{}' is already present in the repository '{}'. Upload url will be skipped for organization/modelName '{}'/'{}'.", requestJson.getOid(), repositoryId, organization, modelName);
                responseJsons.add(requestJson);
                continue;
            }
            String uploadPath = MlModelUtils.getLfsUploadEndpoint(storageId, repositoryId, organization, modelName, requestJson.getOid());
            String signedUrl = String.format("%s%s", baserUrl, uploadPath);
            GitLfsJson lfsUploadJson = createLfsUploadJson(requestJson, request.getHeader("Authorization"), storageId, repositoryId, signedUrl);
            lfsUploadJson.setUploadLink(signedUrl);
            responseJsons.add(lfsUploadJson);
        }
        return new GitLfsBatchJson(responseJsons);
    }

    private boolean tryToReuseExistingSha2(String storageId, String repositoryId, String organization, String modelName, GitLfsJson requestJson) {
        if (storageId == null) {
            throw new NullPointerException("storageId is marked non-null but is null");
        }
        if (repositoryId == null) {
            throw new NullPointerException("repositoryId is marked non-null but is null");
        }
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }


        String lfsTmpUploadPath = MlModelUtils.getLfsTmpUploadPath(organization, modelName, requestJson.getOid());

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, "");
        List<RepositoryPath> artifactList = new ArrayList<>();
        try {
            //todo 性能优化调整成数据库查询
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    if (!file.getFileName().toString().startsWith(".")
                            && !file.getFileName().toString().endsWith(".metadata")
                            && !file.getFileName().toString().endsWith(".md5")
                            && !file.getFileName().toString().endsWith(".sha1")
                            && !file.getFileName().toString().endsWith(".sha256")
                            && file.getFileName().toString().length() == 64) {
                        artifactList.add((RepositoryPath) file);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) {
                    // 处理无法访问的文件
                    log.error("访问文件失败: " + file.toString());
                    exc.printStackTrace();
                    return FileVisitResult.CONTINUE;
                }
            });

        } catch (IOException e) {
            log.error("访问文件失败: " + repositoryPath.toString());
            e.printStackTrace();
        }
        RepositoryPath packageArtifact = artifactList.stream().findFirst().orElse(null);
        boolean found = false;
        if (packageArtifact != null) {
            if (!lfsTmpUploadPath.equals(packageArtifact.getPath())) {
                RepositoryPath tagPath = repositoryPathResolver.resolve(storageId, repositoryId, lfsTmpUploadPath);
                try (InputStream inputStream = Files.newInputStream(packageArtifact);) {
                    artifactManagementService.validateAndStore(tagPath, inputStream);
                } catch (IOException | ProviderImplementationException | ArtifactCoordinatesValidationException e) {
                    throw new RuntimeException(e);
                }
            }
            //ArrayListMultimap arrayListMultimap = ArrayListMultimap.create();
            //arrayListMultimap.put("hf_last_updated", String.valueOf(System.currentTimeMillis()));
            //this.repositoryService.setAttributes(repositoryId, lfsTmpUploadPath, (Multimap)arrayListMultimap);
            found = true;
        }
        return Boolean.valueOf(found);
    }


}
