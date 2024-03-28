package com.veadan.folib.controllers.layout.conan;


import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.ConanInfo;
import com.veadan.folib.domain.ConanPackageInfo;
import com.veadan.folib.domain.ConanRecipeInfo;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.dto.ConanInfoDto;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.DirectoryListingService;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@RestController
@Slf4j
@Api(description = "Conan存储空间控制器", tags = "Conan存储空间控制器")
public class ConanStorageController extends BaseArtifactController {

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/api/conan/info"}, method = {RequestMethod.POST})
    public ResponseEntity<ConanInfo> info(@RequestHeader HttpHeaders httpHeaders,
                                          @RequestBody @Valid ConanInfoDto conanInfoDto,
                                          HttpServletRequest request,
                                          HttpServletResponse response)
            throws Exception {
        final String storageId = conanInfoDto.getStorageId();
        final String repositoryId = conanInfoDto.getRepositoryId();
        final String artifactPath = conanInfoDto.getArtifactPath();
        log.info("Requested get conan info {}/{}/{}.", storageId, repositoryId, artifactPath);
        List<String> list = Arrays.asList(artifactPath.split("/"));
        Integer packageCount = 0;
        String user = list.get(0);
        String name = list.get(1);
        String version = list.get(2);
        String channel = list.get(3);
        String reference = String.format("%s/%s@%s/%s", name, version, user, channel);
        ConanRecipeInfo conanRecipeInfo = ConanRecipeInfo.builder().name(name).version(version).user(user).channel(channel).reference(reference).build();
        String conanFilePath = artifactPath + "/export/conanfile.py";
        RepositoryPath conanFileRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, conanFilePath);
        if (Files.exists(conanFileRepositoryPath)) {
            String conanFileString = Files.readString(conanFileRepositoryPath);
            String author = extractValue(conanFileString, "author\\s*=\\s*\"(.*?)\"");
            conanRecipeInfo.setAuthor(author);
            String license = extractValue(conanFileString, "license\\s*=\\s*\"(.*?)\"");
            conanRecipeInfo.setLicense(license);
            String url = extractValue(conanFileString, "url\\s*=\\s*\"(.*?)\"");
            conanRecipeInfo.setUrl(url);
        }
        String packageParentPath = artifactPath + "/package";
        RepositoryPath packageParentRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, packageParentPath);
        if (Files.exists(packageParentRepositoryPath)) {
            DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(packageParentRepositoryPath);
            packageCount = CollectionUtils.isNotEmpty(directoryListing.getDirectories()) ? directoryListing.getDirectories().size() : 0;
        }
        return ResponseEntity.ok(ConanInfo.builder().recipeInfo(conanRecipeInfo).packageCount(packageCount).build());
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/api/conan/packageInfo"}, method = {RequestMethod.POST})
    public ResponseEntity<ConanPackageInfo> packageInfo(@RequestHeader HttpHeaders httpHeaders,
                                                        @RequestBody @Valid ConanInfoDto conanInfoDto,
                                                        HttpServletRequest request,
                                                        HttpServletResponse response)
            throws Exception {
        final String storageId = conanInfoDto.getStorageId();
        final String repositoryId = conanInfoDto.getRepositoryId();
        final String artifactPath = conanInfoDto.getArtifactPath();
        log.info("Requested get conan package info {}/{}/{}.", storageId, repositoryId, artifactPath);
        ConanPackageInfo conanPackageInfo = null;
        String conanInfoPath = artifactPath + "/conaninfo.txt";
        RepositoryPath conanInfoRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, conanInfoPath);
        if (Files.exists(conanInfoRepositoryPath)) {
            String conanInfoString = Files.readString(conanInfoRepositoryPath);
            Map<String, String> settingsMap = getPackageInfo(conanInfoString, "[settings]");
            Map<String, String> optionsMap = getPackageInfo(conanInfoString, "[options]");
            Map<String, String> allRequiresMap = Maps.newLinkedHashMap();
            Map<String, String> fullRequiresMap = getPackageInfo(conanInfoString, "[full_requires]");
            Map<String, String> requiresMap = getPackageInfo(conanInfoString, "[requires]");
            if (MapUtils.isNotEmpty(fullRequiresMap)) {
                allRequiresMap.putAll(fullRequiresMap);
            }
            if (MapUtils.isNotEmpty(requiresMap)) {
                allRequiresMap.putAll(requiresMap);
            }
            conanPackageInfo = ConanPackageInfo.builder().settings(settingsMap).options(optionsMap).requires(allRequiresMap).build();
        }
        return ResponseEntity.ok(conanPackageInfo);
    }

    private static String extractValue(String input, String patternStr) {
        if (StringUtils.isBlank(input)) {
            return "";
        }
        Pattern pattern = Pattern.compile(patternStr);
        Matcher matcher = pattern.matcher(input);
        if (matcher.find()) {
            String value = matcher.group(1);
            value = value.replaceAll("<.*?>", "");
            return value;
        } else {
            return "";
        }
    }

    private static Map<String, String> getPackageInfo(String content, String key) {
        if (StringUtils.isBlank(content)) {
            return null;
        }
        List<String> requiresKeyList = Lists.newArrayList("[full_requires]", "[requires]");
        boolean flag = false;
        Map<String, String> map = Maps.newLinkedHashMap();
        String[] lines = content.split("\\r?\\n");
        for (String line : lines) {
            if (key.equalsIgnoreCase(line.trim())) {
                flag = true;
                continue;
            } else if (line.trim().startsWith("[")) {
                flag = false;
            }
            if (flag && StringUtils.isNotBlank(line.trim())) {
                if (requiresKeyList.stream().anyMatch(item -> item.equalsIgnoreCase(key))) {
                    map.put(line, "");
                    continue;
                }
                String[] keyValue = line.split("=", 2);
                if (keyValue.length == 2) {
                    String itemKey = keyValue[0].trim();
                    String itemValue = keyValue[1].trim();
                    map.put(itemKey, itemValue);
                }
            }
        }
        return map;
    }
}