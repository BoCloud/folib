package com.veadan.folib.controllers.adapter.jfrog;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.mysql.cj.util.StringUtils;
import com.veadan.folib.controllers.BaseController;
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
@RestController
//@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog拷贝", tags = "JFrog拷贝")
public class ArtifactCopyController extends BaseController
        {
        @Inject
        private   ArtifactPromotionServiceImpl artifactPromotionServiceImp;


        @Inject
        @Qualifier("browseRepositoryDirectoryListingService")
        private volatile DirectoryListingService directoryListingService;
    /**
     * exampleUrl /api/copy/libs-release-local/org/acme?to=/ext-releases-local/org/acme-new&dry=1
     * @param respositryId
     * @param srcFilePath
     * @param to
     * @return
     */
    @ApiOperation(value = "JFrog拷贝")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping("/api/copy/{storageId}/{respositryId}/{srcFilePath:.+}")
    public ResponseEntity<Object> copy(
                                       @PathVariable("storageId") String storageId,
                                       @PathVariable("respositryId") String respositryId,
                                       @PathVariable("srcFilePath") String srcFilePath,
                                       String to,
                                       String dry) {
        Map<String,Object> result=new HashMap<>();
        List<JSONObject> infoList=new ArrayList<>();
        JSONObject jsonObject=new JSONObject();
        try {
            log.info("制品copy接口调用，参数respositryId:{};参数srcFilePath:{};参数to:{};参数dry:{}", respositryId, srcFilePath, to, dry);
            String srcStorageId = storageId;
            String srcrespositryId = respositryId;
            // 解析目标地址 目录地址必须是/开始
            String[] targetStrs = to.split("/");
            String targetStorageId = targetStrs[1];
            String targetRepositoryId = targetStrs[2];
            ArtifactPromotion artifactPromotion = new ArtifactPromotion();
            artifactPromotion.setPath(srcFilePath);
            artifactPromotion.setSrcStorageId(srcStorageId);
            artifactPromotion.setSrcRepositoryId(srcrespositryId);
            List<TargetRepositoyDto> list = new ArrayList<>();
            TargetRepositoyDto targetRepositoyDto = new TargetRepositoyDto();
            targetRepositoyDto.setTargetStorageId(targetStorageId);
            targetRepositoyDto.setTargetRepositoryId(targetRepositoryId);
            list.add(targetRepositoyDto);
            artifactPromotion.setTargetRepositoyList(list);
            ResponseEntity responseEntity = artifactPromotionServiceImp.copy(artifactPromotion);
            if(responseEntity.getStatusCode().value() == 200){
                jsonObject.put("level","info");
                jsonObject.put("message","copying "+storageId+"/"+respositryId+"/"+srcFilePath+" to "+to+" completed successfully");
                infoList.add(jsonObject);
                result.put("messages",infoList);
                return ResponseEntity.ok(result);
            }else {
                jsonObject.put("level","error");
                jsonObject.put("message","copying "+storageId+"/"+respositryId+"/"+srcFilePath+" to "+to+" fail "+ responseEntity.getStatusCode());
                infoList.add(jsonObject);
                result.put("messages",infoList);
                return ResponseEntity.ok(result);
            }
        }catch (Exception exception){
            jsonObject.put("level","error");
            jsonObject.put("message","copying "+storageId+"/"+respositryId+"/"+srcFilePath+" to "+to+" fail "+ exception.getMessage());
            infoList.add(jsonObject);
            result.put("messages",infoList);
            return ResponseEntity.ok(result);
        }

    }

    /**
     *  exampleUrl POST api/docker/public-project/docker-local/v2/promote
     * {
     *   "targetRepo": "docker-prod",
     *   "dockerRepository": "jfrog/ubuntu"
     * }
     * @param respositryId
     * @param dockerCopyDto
     * @return
     */
    @ApiOperation(value = "JFrog镜像拷贝")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @RequestMapping("/api/docker/{storageId}/{respositryId}/v2/promote")
    public ResponseEntity<Object> dockerCopy(@PathVariable("storageId") String storageId,@PathVariable("respositryId") String respositryId, @RequestBody DockerCopyDto dockerCopyDto) {
        log.info("docker 制品晋级(copy)接口调用，参数{}实体{}", respositryId, JSONObject.toJSONString(dockerCopyDto));
        String srcStorageId = storageId;
        String srcRespositryId = respositryId;
        String srcFilePath=dockerCopyDto.getDockerRepository();
        String targerStorageId = dockerCopyDto.getTargetStorageId();
        String tagetRespositryId = dockerCopyDto.getTargetDockerRepository();
        List<JSONObject> infoList = new ArrayList<>();
        JSONObject jsonObject = new JSONObject();
        List<String> tagList=new ArrayList<>();
        // 如果有带tag号 这边默认时晋级最新的
        if(!StringUtils.isNullOrEmpty(dockerCopyDto.getTag())){
            tagList.add(dockerCopyDto.getTag());
        }else{
            //查找所有的tag号，将所有的tag全部晋级到姆目标仓库
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(srcStorageId, srcRespositryId, dockerCopyDto.getDockerRepository());
            try {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                List<FileContent> imageDirList = directoryListing.getDirectories().stream().filter(f -> (!f.getName().equals("blobs")) && (!f.getName().equals("manifest"))).collect(Collectors.toList());
                imageDirList.forEach(item->{
                    log.info("晋级镜像版本{}",item.getName());
                    tagList.add(item.getName());
                });
            } catch (IOException e) {
                jsonObject.put("info", "error");
                jsonObject.put("message", "镜像版本失败获取失败");
                return ResponseEntity.ok(jsonObject);
            }
        }
        // 这里已经获取到所有的镜像tag 循环上传
        for(String tag:tagList) {
            try {
                ArtifactPromotion artifactPromotion = new ArtifactPromotion();
                artifactPromotion.setPath(srcFilePath + "/" + tag);
                artifactPromotion.setSrcStorageId(srcStorageId);
                artifactPromotion.setSrcRepositoryId(srcRespositryId);
                List<TargetRepositoyDto> list = new ArrayList<>();
                TargetRepositoyDto targetRepositoyDto = new TargetRepositoyDto();
                targetRepositoyDto.setTargetStorageId(targerStorageId);
                targetRepositoyDto.setTargetRepositoryId(tagetRespositryId);
                list.add(targetRepositoyDto);
                artifactPromotion.setTargetRepositoyList(list);
                ResponseEntity responseEntity = artifactPromotionServiceImp.copy(artifactPromotion);

                if (responseEntity.getStatusCode().value() == 200) {
                    jsonObject.put("level", "info");
                    jsonObject.put("message", "copying " + storageId + "/" + respositryId + "/" + srcFilePath + " to " + jsonObject.toJSONString(artifactPromotion) + " completed successfully");
                    infoList.add(jsonObject);
                } else {
                    jsonObject.put("level", "error");
                    jsonObject.put("message", "copying " + storageId + "/" + respositryId + "/" + srcFilePath + " to " + jsonObject.toJSONString(artifactPromotion) + " fail " + responseEntity.getStatusCode());
                    infoList.add(jsonObject);
                }
            } catch (Exception exception) {
                jsonObject.put("level", "error");
                jsonObject.put("message", "copying " + storageId + "/" + respositryId + "/" + srcFilePath + " to " + jsonObject.toJSONString(dockerCopyDto) + " fail " + exception.getMessage());
                infoList.add(jsonObject);
            }

        }
        return ResponseEntity.ok(infoList);
    }
}
