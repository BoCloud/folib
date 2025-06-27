package com.veadan.folib.controllers.adapter.jfrog;

import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.adapter.jfrog.req.ResolveBatchPathReq;
import com.veadan.folib.controllers.adapter.jfrog.req.ResolvePathFiles;
import com.veadan.folib.controllers.adapter.jfrog.res.BatchDownloadRes;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.util.RepositoryPathUtil;
import io.swagger.annotations.Api;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;


import java.io.File;
import java.net.URL;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 */
@Slf4j
@RequestMapping("/artifactory")
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog下载", tags = "JFrog下载")
public class ArtifactDownController extends JFrogBaseController {

    @PreAuthorize("authenticated")
    @GetMapping(value = "/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity download(@PathVariable("repositoryId") String repositoryId, @RequestHeader HttpHeaders httpHeaders, @PathVariable String artifactPath,
                                   HttpServletRequest request, HttpServletResponse response) throws Exception {
        final String storageId = getDefaultStorageId(repositoryId);
        boolean checkRepository = checkRepository(storageId, repositoryId);
        if (!checkRepository) {
            return repositoryNotFound();
        }
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, artifactPath);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);

        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        return null;
    }

    @PreAuthorize("authenticated")
    @PostMapping(value = "/resolveBatchPath")
    public ResponseEntity<Object> resolveBatchPath(@RequestBody @Validated ResolveBatchPathReq resolveBatchPathReq, HttpServletRequest request, HttpServletResponse response) throws Exception {
        if (Objects.isNull(resolveBatchPathReq) || CollectionUtils.isEmpty(resolveBatchPathReq.getFiles())) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("Parameter files data error, please check");
        }
        List<ResolvePathFiles> files = resolveBatchPathReq.getFiles();
        String pattern, target, tempTarget, storageId, repositoryId, artifactPath;
        Boolean recursive, flat;
        String[] arr;
        List<BatchDownloadRes> batchDownloadResList = Lists.newArrayList();
        for (ResolvePathFiles resolvePathFiles : files) {
            pattern = resolvePathFiles.getPattern();
            pattern = StringUtils.removeStart(pattern, File.separator);
            pattern = StringUtils.removeEnd(pattern, File.separator);
            arr = pattern.split(File.separator);
            if (arr.length < 3) {
                logger.info("Parameter pattern data error, please check");
                continue;
            }
            storageId = arr[0];
            repositoryId = arr[1];
            Storage storage = configurationManager.getStorage(storageId);
            if (Objects.isNull(storage)) {
                logger.info(GlobalConstants.STORAGE_NOT_FOUND_MESSAGE);
                continue;
            }
            if (Objects.isNull(storage.getRepository(repositoryId))) {
                logger.info(GlobalConstants.REPOSITORY_NOT_FOUND_MESSAGE);
                continue;
            }
            target = resolvePathFiles.getTarget();
            target = StringUtils.removeEnd(target, File.separator);
            recursive = resolvePathFiles.getRecursive();
            flat = resolvePathFiles.getFlat();
            RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            List<RepositoryPath> repositoryPathList = RepositoryPathUtil.getGlobPaths(rootRepositoryPath.getRepository().getLayout(), rootRepositoryPath, pattern, recursive, null);
            if (CollectionUtils.isEmpty(repositoryPathList)) {
                continue;
            }
            for (RepositoryPath repositoryPath : repositoryPathList) {
                URL artifactResource = RepositoryFiles.readResourceUrl(repositoryPath);
                BatchDownloadRes batchDownloadRes = BatchDownloadRes.builder().url(artifactResource.toString()).build();
                if (Boolean.TRUE.equals(flat)) {
                    tempTarget = target + File.separator + RepositoryFiles.relativizePath(repositoryPath);
                } else {
                    tempTarget = target + File.separator + repositoryPath.getFileName().toString();
                }
                batchDownloadRes.setTarget(tempTarget);
                batchDownloadResList.add(batchDownloadRes);
            }
        }
        return ResponseEntity.ok(batchDownloadResList);
    }

}
