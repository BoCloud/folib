package com.veadan.folib.controllers.adapter.jfrog;

import com.alibaba.fastjson.JSONObject;
import com.mysql.cj.util.StringUtils;
import com.veadan.folib.controllers.adapter.jfrog.dto.DockerCopyDto;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.domain.FileContent;
import com.veadan.folib.dto.TargetRepositoyDto;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.DirectoryListingService;
import com.veadan.folib.services.impl.ArtifactPromotionServiceImpl;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * 需要验证仓库的唯一性
 */
@Slf4j
@RequestMapping("/artifactory")
@RestController
//@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog拷贝", tags = "JFrog拷贝")
public class ArtifactCopyController extends JFrogBaseController {
    @Inject
    private ArtifactPromotionServiceImpl artifactPromotionServiceImp;


    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    /**
     * exampleUrl /api/copy/libs-release-local/org/acme?to=/ext-releases-local/org/acme-new&dry=1
     *
     * @param repositoryId
     * @param artifactPath
     * @param to
     * @return
     */
    @ApiOperation(value = "JFrog拷贝")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping("/api/copy/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<Object> copy(@PathVariable("repositoryId") String repositoryId,
                                       @PathVariable("artifactPath") String artifactPath,
                                       String to,
                                       String dry) throws Exception {
        String storageId = getDefaultStorageId();
        boolean checkRepository = checkRepository(storageId, repositoryId);
        if (!checkRepository) {
            return repositoryNotFound("source");
        }
        Map<String, Object> result = new HashMap<>();
        List<JSONObject> infoList = new ArrayList<>();
        JSONObject jsonObject = new JSONObject();
        try {
            log.info("制品copy接口调用，参数respositryId:{};参数artifactPath:{};参数to:{};参数dry:{}", repositoryId, artifactPath, to, dry);
            // 解析目标地址 目录地址必须是/开始
            if (!to.startsWith("/")) {
                to = "/" + to;
            }
            String[] targetStrs = to.split("/");
            String targetRepositoryId = targetStrs[1];
            checkRepository = checkRepository(storageId, targetRepositoryId);
            if (!checkRepository) {
                return repositoryNotFound("target");
            }
            ArtifactPromotion artifactPromotion = new ArtifactPromotion();
            artifactPromotion.setPath(artifactPath);
            artifactPromotion.setSrcStorageId(storageId);
            artifactPromotion.setSrcRepositoryId(repositoryId);
            List<TargetRepositoyDto> list = new ArrayList<>();
            TargetRepositoyDto targetRepositoyDto = new TargetRepositoyDto();
            targetRepositoyDto.setTargetStorageId(storageId);
            targetRepositoyDto.setTargetRepositoryId(targetRepositoryId);
            list.add(targetRepositoyDto);
            artifactPromotion.setTargetRepositoyList(list);
            ResponseEntity responseEntity = artifactPromotionServiceImp.copy(artifactPromotion);
            if (responseEntity.getStatusCode().value() == 200) {
                jsonObject.put("level", "info");
                jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + to + " completed successfully");
                infoList.add(jsonObject);
                result.put("messages", infoList);
                return ResponseEntity.ok(result);
            } else {
                jsonObject.put("level", "error");
                jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + to + " fail " + responseEntity.getStatusCode());
                infoList.add(jsonObject);
                result.put("messages", infoList);
                return ResponseEntity.ok(result);
            }
        } catch (Exception exception) {
            jsonObject.put("level", "error");
            jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + to + " fail " + exception.getMessage());
            infoList.add(jsonObject);
            result.put("messages", infoList);
            return ResponseEntity.ok(result);
        }

    }

    /**
     * exampleUrl POST api/docker/public-project/docker-local/v2/promote
     * {
     * "targetRepo": "docker-prod",
     * "dockerRepository": "jfrog/ubuntu"
     * }
     *
     * @param repositoryId
     * @param dockerCopyDto
     * @return
     */
    @ApiOperation(value = "JFrog镜像拷贝")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping("/api/docker/{repositoryId}/v2/promote")
    public ResponseEntity<Object> dockerCopy(@PathVariable("repositoryId") String repositoryId, @RequestBody DockerCopyDto dockerCopyDto) {
        log.info("docker 制品晋级(copy)接口调用，参数{}实体{}", repositoryId, JSONObject.toJSONString(dockerCopyDto));
        String storageId = getDefaultStorageId();
        boolean checkRepository = checkRepository(storageId, repositoryId);
        if (!checkRepository) {
            return repositoryNotFound("source");
        }
        String imageTag = dockerCopyDto.getTag();
        String artifactPath = dockerCopyDto.getDockerRepository();
        String split = "/";
        if (artifactPath.contains(split)) {
            String[] artifactPathArr = artifactPath.split(split);
            artifactPath = artifactPathArr[0];
            imageTag = artifactPathArr[1];
        }
        String targetRepositoryId = dockerCopyDto.getTargetRepo();
        checkRepository = checkRepository(storageId, targetRepositoryId);
        if (!checkRepository) {
            return repositoryNotFound("target");
        }
        List<JSONObject> infoList = new ArrayList<>();
        JSONObject jsonObject = new JSONObject();
        List<String> tagList = new ArrayList<>();
        // 如果有带tag号 这边默认时晋级最新的
        if (!StringUtils.isNullOrEmpty(imageTag)) {
            tagList.add(imageTag);
        } else {
            //查找所有的tag号，将所有的tag全部晋级到目标仓库
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, dockerCopyDto.getDockerRepository());
            try {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                List<FileContent> imageDirList = directoryListing.getDirectories().stream().filter(f -> (!f.getName().equals("blobs")) && (!f.getName().equals("manifest"))).collect(Collectors.toList());
                imageDirList.forEach(item -> {
                    log.info("晋级镜像版本{}", item.getName());
                    tagList.add(item.getName());
                });
            } catch (IOException e) {
                jsonObject.put("info", "error");
                jsonObject.put("message", "镜像版本失败获取失败");
                return ResponseEntity.ok(jsonObject);
            }
        }
        // 这里已经获取到所有的镜像tag 循环上传
        for (String tag : tagList) {
            try {
                ArtifactPromotion artifactPromotion = new ArtifactPromotion();
                artifactPromotion.setPath(artifactPath + "/" + tag);
                artifactPromotion.setSrcStorageId(storageId);
                artifactPromotion.setSrcRepositoryId(repositoryId);
                List<TargetRepositoyDto> list = new ArrayList<>();
                TargetRepositoyDto targetRepositoyDto = new TargetRepositoyDto();
                targetRepositoyDto.setTargetStorageId(storageId);
                targetRepositoyDto.setTargetRepositoryId(targetRepositoryId);
                list.add(targetRepositoyDto);
                artifactPromotion.setTargetRepositoyList(list);
                ResponseEntity responseEntity = artifactPromotionServiceImp.copy(artifactPromotion);

                if (responseEntity.getStatusCode().value() == 200) {
                    jsonObject.put("level", "info");
                    jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + JSONObject.toJSONString(artifactPromotion) + " completed successfully");
                    infoList.add(jsonObject);
                } else {
                    jsonObject.put("level", "error");
                    jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + JSONObject.toJSONString(artifactPromotion) + " fail " + responseEntity.getStatusCode());
                    infoList.add(jsonObject);
                }
            } catch (Exception exception) {
                jsonObject.put("level", "error");
                jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + JSONObject.toJSONString(dockerCopyDto) + " fail " + exception.getMessage());
                infoList.add(jsonObject);
            }

        }
        return ResponseEntity.ok(infoList);
    }
}
