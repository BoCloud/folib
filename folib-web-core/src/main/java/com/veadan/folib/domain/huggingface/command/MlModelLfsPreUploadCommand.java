package com.veadan.folib.domain.huggingface.command;

import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.gitls.command.LfsBaseUploadCommand;
import com.veadan.folib.domain.gitls.model.GitLfsBatchJson;
import com.veadan.folib.domain.gitls.model.GitLfsJson;
import com.veadan.folib.domain.gitls.utils.GitLfsHelper;
import com.veadan.folib.domain.huggingface.utils.MlModelUtils;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.util.RepositoryPathUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

public class MlModelLfsPreUploadCommand extends LfsBaseUploadCommand {

    private static final Logger log = LoggerFactory.getLogger(MlModelLfsPreUploadCommand.class);

    static final long EXPIRATION_TIME_FOR_SIGNED_URL_SEC = 600L;

    public MlModelLfsPreUploadCommand(ArtifactRepository artifactRepository, ConfigurationManager configurationManager) {
        super(artifactRepository, configurationManager);
    }

    public GitLfsBatchJson preUploadBatch(String storageId,String repositoryId, String organization, String modelName, GitLfsBatchJson batchLfsJson, HttpServletRequest request) {
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
        baserUrl = baserUrl.endsWith("/") ? baserUrl.substring(0, baserUrl.length() - 1)+"/storages" : baserUrl+"/storages";
        for (GitLfsJson requestJson : batchLfsJson.getObjects()) {
            //boolean sha2ReusePossible = tryToReuseExistingSha2(repoKey, organization, modelName, requestJson);
            //if (sha2ReusePossible) {
            //    log.debug("Same sha2 '{}' is already present in the repository '{}'. Upload url will be skipped for organization/modelName '{}'/'{}'.",  requestJson.getOid(), repoKey, organization, modelName);
            //    responseJsons.add(requestJson);
            //    continue;
            //}
            // TODO: 2024/6/6
            String uploadPath = MlModelUtils.getLfsUploadEndpoint(storageId,repositoryId, organization, modelName, requestJson.getOid());
            String signedUrl = String.format("%s%s", baserUrl, uploadPath);
            GitLfsJson lfsUploadJson = createLfsUploadJson(requestJson, request.getHeader("Authorization"), storageId,repositoryId, signedUrl);
            lfsUploadJson.setUploadLink(signedUrl);
            responseJsons.add(lfsUploadJson);
        }
        return new GitLfsBatchJson(responseJsons);
    }

    //private boolean tryToReuseExistingSha2( String repoKey,  String organization,  String modelName, GitLfsJson requestJson){
    //    if (repoKey == null) {
    //        throw new NullPointerException("repoKey is marked non-null but is null");
    //    }
    //    if (modelName == null) {
    //        throw new NullPointerException("modelName is marked non-null but is null");
    //    }
    //    //todo 待实现
    //    return ((Boolean)this.packageHandlerService.securityService().callAsSystem(() -> {
    //        String lfsTmpUploadPath = MlModelUtils.getLfsTmpUploadPath(organization, modelName, requestJson.getOid());
    //        Stream<PackageArtifact> artifactsForSystemUser = this.searchService.findArtifactsBySha2Checksum(repoKey, requestJson.getOid());
    //        PackageArtifact packageArtifact = artifactsForSystemUser.findFirst().orElse(null);
    //        boolean found = false;
    //        if (packageArtifact != null) {
    //            if (!lfsTmpUploadPath.equals(packageArtifact.getPath()))
    //                this.repositoryService.copy(packageArtifact.getRepoKey(), repoKey, packageArtifact.getPath(), lfsTmpUploadPath);
    //            ArrayListMultimap arrayListMultimap = ArrayListMultimap.create();
    //            arrayListMultimap.put("hf_last_updated", String.valueOf(System.currentTimeMillis()));
    //            this.repositoryService.setAttributes(repoKey, lfsTmpUploadPath, (Multimap)arrayListMultimap);
    //            found = true;
    //        }
    //        return Boolean.valueOf(found);
    //    })).booleanValue();
    //
    //    List<Path> tagList = Lists.newArrayList();
    //    if (Objects.isNull(currentManifestPath)) {
    //        //当前版本下manifest文件信息
    //        Files.walkFileTree(repositoryPath, new SimpleFileVisitor<Path>() {
    //            @Override
    //            public FileVisitResult visitFile(Path file,
    //                                             BasicFileAttributes attrs)
    //                    throws IOException {
    //                if (DockerArtifactCoordinates.isTagPath(file)) {
    //                    tagList.add(file);
    //                }
    //                return FileVisitResult.CONTINUE;
    //            }
    //
    //            @Override
    //            public FileVisitResult postVisitDirectory(Path dir,
    //                                                      IOException exc)
    //                    throws IOException {
    //                RepositoryPath itemPath = (RepositoryPath) dir;
    //                if (!RepositoryPathUtil.include(2, itemPath, true)) {
    //                    log.debug("RepositoryPath [{}] skip...", itemPath.toString());
    //                    return FileVisitResult.SKIP_SUBTREE;
    //                }
    //                return FileVisitResult.CONTINUE;
    //            }
    //        });
    //    } else {
    //        tagList.add(currentManifestPath);
    //    }
    //
    //    return false;
    //}
}
