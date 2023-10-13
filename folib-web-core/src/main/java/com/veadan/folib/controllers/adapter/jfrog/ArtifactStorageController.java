package com.veadan.folib.controllers.adapter.jfrog;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.adapter.jfrog.ArtifactStorageInfo;
import com.veadan.folib.enums.ArtifactMetadataEnum;
import com.veadan.folib.forms.artifact.ArtifactMetadataForm;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactWebService;
import com.veadan.folib.storage.Storage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.activation.MimetypesFileTypeMap;
import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.time.ZoneId;
import java.util.*;

/**
 * @author leipenghui
 */
@Slf4j
@RestController
@RequestMapping("/artifactory/api/storage")
//@PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
@Api(description = "JFrog存储", tags = "JFrog存储")
public class ArtifactStorageController extends JFrogBaseController {

    private static final String NOT_FOUND_MESSAGE = "No properties could be found.";

    private static final String ARTIFACT_NOT_FOUND_MESSAGE = "The artifact was not found.";

    private static final String STORAGE_NOT_FOUND_MESSAGE = "The storage was not found.";

    private static final String REPOSITORY_NOT_FOUND_MESSAGE = "The repository was not found.";

    private static final String PROPERTIES_VALUE_CANNOT_BE_EMPTY = "Properties value cannot be empty.";

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactWebService artifactWebService;

    @ApiOperation(value = "JFrog存储")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @RequestMapping(value = {"/{repositoryId}/{artifactPath:.+}"}, method = {RequestMethod.GET})
    public ResponseEntity<Object> itemProperties(@PathVariable("repositoryId") String repositoryId, @PathVariable("artifactPath") String artifactPath,
                                                 @RequestParam(value = "properties", required = false) String properties, HttpServletRequest request) throws Exception {
        String storageId = getDefaultStorageId();
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, REPOSITORY_NOT_FOUND_MESSAGE));
        }
        Artifact artifact = findArtifact(storageId, repositoryId, artifactPath);
        if (Objects.isNull(artifact)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, ARTIFACT_NOT_FOUND_MESSAGE));
        }
        String propertiesKey = "properties";
        boolean hasPropertiesKey = request.getParameterMap().containsKey(propertiesKey);
        String metadata = artifact.getMetadata();
        if (hasPropertiesKey && StringUtils.isBlank(metadata)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, null));
        }
        ArtifactStorageInfo artifactStorageInfo = ArtifactStorageInfo.builder().uri(request.getRequestURL().toString()).build();
        if (hasPropertiesKey && StringUtils.isNotBlank(metadata)) {
            List<String> propertyList = null, valueList = null;
            if (StringUtils.isNotBlank(properties)) {
                propertyList = Arrays.asList(properties.split(","));
            }
            JSONObject metadataJson = JSONObject.parseObject(metadata);
            if (CollectionUtils.isNotEmpty(propertyList) && metadataJson.keySet().stream().noneMatch(propertyList::contains)) {
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, null));
            }
            Map<String, Object> propertiesMap = Maps.newLinkedHashMap();
            String value;
            for (String key : metadataJson.keySet()) {
                value = metadataJson.getJSONObject(key).getString("value");
                if (StringUtils.isBlank(value)) {
                    propertiesMap.put(key, "");
                    continue;
                }
                valueList = Arrays.asList(value.split(","));
                if (CollectionUtils.isNotEmpty(propertyList)) {
                    if (propertyList.contains(key)) {
                        propertiesMap.put(key, valueList);
                    }
                } else {
                    propertiesMap.put(key, valueList);
                }
                artifactStorageInfo.setProperties(propertiesMap);
            }
        }
        if (!hasPropertiesKey) {
            String admin = "admin";
            artifactStorageInfo.setRepo(String.format("%s/%s", artifact.getStorageId(), artifact.getRepositoryId()));
            artifactStorageInfo.setPath("/" + artifact.getArtifactPath());
            artifactStorageInfo.setCreated(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
            artifactStorageInfo.setCreatedBy(admin);
            artifactStorageInfo.setLastModified(Date.from(artifact.getLastUpdated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
            artifactStorageInfo.setModifiedBy(admin);
            artifactStorageInfo.setLastUpdated(artifactStorageInfo.getLastModified());
            artifactStorageInfo.setDownloadUri(String.format("%s/%s", getBaseUrl(storageId, repositoryId), artifact.getArtifactPath()));
            MimetypesFileTypeMap mimetypesFileTypeMap = new MimetypesFileTypeMap();
            String mimeType = mimetypesFileTypeMap.getContentType(artifact.getArtifactPath());
            artifactStorageInfo.setMimeType(mimeType);
            artifactStorageInfo.setSize(Objects.nonNull(artifact.getSizeInBytes()) ? artifact.getSizeInBytes().toString() : "0");
            Map<String, String> checksumsMap = artifact.getChecksums();
            artifactStorageInfo.setChecksums(replaceKey(checksumsMap));
            artifactStorageInfo.setOriginalChecksums(artifactStorageInfo.getChecksums());
        }
        return ResponseEntity.ok(artifactStorageInfo);
    }

    @ApiOperation(value = "JFrog存储")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PutMapping(value = {"/{repositoryId}/{artifactPath:.+}"})
    public ResponseEntity<Object> setItemProperties(@PathVariable("repositoryId") String repositoryId, @PathVariable("artifactPath") String artifactPath,
                                                    @RequestParam(value = "properties", required = false) String properties, HttpServletRequest request) throws Exception {

        String storageId = getDefaultStorageId();
        Storage storage = getStorage(storageId);
        if (StringUtils.isBlank(properties)) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(handlerErrors(HttpStatus.BAD_REQUEST.value(), PROPERTIES_VALUE_CANNOT_BE_EMPTY));
        }
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, REPOSITORY_NOT_FOUND_MESSAGE));
        }
        Artifact artifact = findArtifact(storageId, repositoryId, artifactPath);
        if (Objects.isNull(artifact)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, ARTIFACT_NOT_FOUND_MESSAGE));
        }
        String splitSemicolon = ";", splitVerticalLine = "|", splitVerticalLineEncode = "\\|", splitSlashSemicolon = "\\;", splitSlashSemicolonEncode = "\\\\;", splitComma = ",";
        List<String> propertyList = Arrays.asList(properties.split(splitSemicolon)), itemPropertyList = null, propertyGroupSplitList = null;
        if (properties.contains(splitVerticalLine)) {
            propertyList = Arrays.asList(properties.split(splitVerticalLineEncode));
        }
        List<ArtifactMetadataForm> artifactMetadataFormList = Lists.newArrayList();
        ArtifactMetadataForm artifactMetadataForm = null;
        String metadataKey, metadataValue;
        if (CollectionUtils.isNotEmpty(propertyList)) {
            for (String propertyGroup : propertyList) {
                propertyGroupSplitList = Arrays.asList(propertyGroup.split("="));
                metadataKey = propertyGroupSplitList.get(0);
                if (propertyGroupSplitList.size() >= 2) {
                    metadataValue = propertyGroupSplitList.get(1);
                } else {
                    metadataValue = "";
                }
                artifactMetadataForm = ArtifactMetadataForm.builder().storageId(storageId).repositoryId(repositoryId).artifactPath(artifactPath).type(ArtifactMetadataEnum.STRING.toString()).key(metadataKey).viewShow(1).build();
                itemPropertyList = Arrays.asList(metadataValue.split(splitComma));
                if (metadataValue.contains(splitSlashSemicolon)) {
                    itemPropertyList = Arrays.asList(metadataValue.split(splitSlashSemicolonEncode));
                }
                if (CollectionUtils.isNotEmpty(itemPropertyList)) {
                    artifactMetadataForm.setValue(String.join(",", itemPropertyList));
                }
                artifactMetadataFormList.add(artifactMetadataForm);
            }
            artifactWebService.batchArtifactMetadata(artifactMetadataFormList);
        }
        return ResponseEntity.status(HttpStatus.NO_CONTENT.value()).body("");
    }

    /**
     * 替换key
     *
     * @param checksumsMap 源map
     * @return 新map
     */
    private Map<String, String> replaceKey(Map<String, String> checksumsMap) {
        Map<String, String> newChecksumsMap = Maps.newLinkedHashMap();
        Map<String, String> replacementMap = Maps.newHashMap();
        replacementMap.put("SHA-1", "sha1");
        replacementMap.put("SHA-256", "sha256");
        replacementMap.put("SHA-512", "sha512");
        replacementMap.put("MD5", "md5");
        for (Map.Entry<String, String> entry : checksumsMap.entrySet()) {
            String key = entry.getKey();
            key = replacementMap.getOrDefault(key, key);
            newChecksumsMap.put(key, entry.getValue());
        }
        return newChecksumsMap;
    }

}
