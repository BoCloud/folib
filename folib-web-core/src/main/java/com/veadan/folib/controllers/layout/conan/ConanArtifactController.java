package com.veadan.folib.controllers.layout.conan;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.*;
import com.veadan.folib.dto.ConanInfoDto;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ConanArtifactServer;
import com.veadan.folib.services.DirectoryListingService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.security.JwtAuthenticationClaimsProvider;
import com.veadan.folib.users.security.JwtClaimsProvider;
import com.veadan.folib.users.security.SecurityTokenProvider;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.web.Constants;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

//@LayoutRequestMapping("conan")
@RestController
@Slf4j
@Api(description = "Conan坐标控制器",tags = "Conan坐标控制器")
public class ConanArtifactController extends BaseArtifactController {

    @Autowired
    private ConanArtifactServer conanArtifactServer;

    @Autowired
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Inject
    @JwtAuthenticationClaimsProvider.JwtAuthentication
    private JwtClaimsProvider jwtClaimsProvider;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/ping")
    public ResponseEntity ping(@RequestHeader HttpHeaders httpHeaders,
                               HttpServletRequest request, HttpServletResponse response) {
        return new ResponseEntity<>("ok", HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/digest")
    public ResponseEntity exportDigest(@RepositoryMapping Repository repository,
                                       @PathVariable("name") String name,
                                       @PathVariable("version") String version,
                                       @PathVariable("user") String user,
                                       @PathVariable("channel") String channel) throws IOException {
        String path = String.format("%s/%s/%s/%s/0/export/conanmanifest.txt", user, name, version, channel);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), path);
        if (null == repositoryPath || !Files.exists(repositoryPath)) {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
        }
        String exportManifestUrl = getBaseUrl(repository) + "/v1/files/" + path;
        Map<String, String> map = Map.of("conanmanifest.txt", exportManifestUrl);
        return new ResponseEntity<>(map, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/packages/{packageId}/digest")
    public ResponseEntity packagesDigest(@RepositoryMapping Repository repository,
                                         @PathVariable("name") String name,
                                         @PathVariable("version") String version,
                                         @PathVariable("user") String user,
                                         @PathVariable("channel") String channel,
                                         @PathVariable("packageId") String packageId) throws IOException {
        String packagePath = String.format("%s/%s/%s/%s/0/package/%s/0/conanmanifest.txt", user, name, version, channel, packageId);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), packagePath);
        if (null == repositoryPath || !Files.exists(repositoryPath)) {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
        }
        String packageManifestUrl = getBaseUrl(repository) + "/v1/files/" + packagePath;
        Map<String, String> map = Map.of("conanmanifest.txt", packageManifestUrl);
        return new ResponseEntity<>(map, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/users/authenticate")
    public ResponseEntity userAuthenticate(Authentication authentication,
                                           @RequestHeader(HttpHeaders.ACCEPT) String accept,
                                           @RequestHeader HttpHeaders httpHeaders,
                                           HttpServletRequest request, HttpServletResponse response) throws Exception {

        if (Objects.nonNull(authentication) && authentication.getPrincipal() instanceof SpringSecurityUser) {
            SpringSecurityUser springSecurityUser = (SpringSecurityUser) authentication.getPrincipal();
            Map<String, String> claimMap = jwtClaimsProvider.getClaims(springSecurityUser);
            int expireSeconds = 2626560;
            String token = securityTokenProvider.getToken(springSecurityUser.getUsername(), claimMap, expireSeconds, null);
            return ResponseEntity.ok(token);
        }
        return new ResponseEntity<>("ok", HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/users/check_credentials")
    public ResponseEntity checkCredentials(Authentication authentication,
                                           @RequestHeader(HttpHeaders.ACCEPT) String accept,
                                           @RequestHeader HttpHeaders httpHeaders,
                                           HttpServletRequest request, HttpServletResponse response) {
        return new ResponseEntity<>("ok", HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}")
    public ResponseEntity checkExport(@RepositoryMapping Repository repository,
                                      @PathVariable("name") String name,
                                      @PathVariable("version") String version,
                                      @PathVariable("user") String user,
                                      @PathVariable("channel") String channel) throws Exception {
        Map<String, String> dataMap = Maps.newHashMap();
        String prefix = String.format("%s/%s/%s/%s/0/export", user, name, version, channel);
        String conanSources = "conan_sources.tgz", conanFile = "conanfile.py", conanManifest = "conanmanifest.txt";
        String conanSourcesPath = String.format("%s/%s", prefix, conanSources);
        String conanFilePath = String.format("%s/%s", prefix, conanFile);
        String conanManifestPath = String.format("%s/%s", prefix, conanManifest);
        RepositoryPath conanSourcesRepositoryPath = artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), conanSourcesPath);
        if (Objects.nonNull(conanSourcesRepositoryPath) && Objects.nonNull(conanSourcesRepositoryPath.getArtifactEntry())) {
            dataMap.put(conanSources, conanSourcesRepositoryPath.getArtifactEntry().getChecksums().getOrDefault(MessageDigestAlgorithms.MD5, ""));
        }
        RepositoryPath conanFileRepositoryPath = artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), conanFilePath);
        if (Objects.nonNull(conanFileRepositoryPath) && Objects.nonNull(conanFileRepositoryPath.getArtifactEntry())) {
            dataMap.put(conanFile, conanFileRepositoryPath.getArtifactEntry().getChecksums().getOrDefault(MessageDigestAlgorithms.MD5, ""));
        }
        RepositoryPath conanManifestRepositoryPath = artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), conanManifestPath);
        if (Objects.nonNull(conanManifestRepositoryPath) && Objects.nonNull(conanManifestRepositoryPath.getArtifactEntry())) {
            dataMap.put(conanManifest, conanManifestRepositoryPath.getArtifactEntry().getChecksums().getOrDefault(MessageDigestAlgorithms.MD5, ""));
        }
        if (MapUtils.isEmpty(dataMap)) {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
        }
        return ResponseEntity.ok(dataMap);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/search")
    public ResponseEntity search(
            @RepositoryMapping Repository repository,
            @RequestParam(value = "q", required = false) String query) throws IOException {
        return conanArtifactServer.searchConanPackage(repository, query);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @PostMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/upload_urls")
    public ResponseEntity uploadExportUrls(@RepositoryMapping Repository repository,
                                           @PathVariable("name") String name,
                                           @PathVariable("version") String version,
                                           @PathVariable("user") String user,
                                           @PathVariable("channel") String channel,
                                           @RequestBody(required = false) LinkedHashMap<String, String> obj) {
        if (obj == null) {
            return new ResponseEntity<>("", HttpStatus.NOT_FOUND);
        }

        String url = getBaseUrl(repository);
        obj.entrySet().forEach(entry -> {
            String packageName = entry.getKey();
            entry.setValue(String.format("%s/v1/files/%s/%s/%s/%s/0/export/%s", url, user, name, version, channel, packageName));
        });
        return new ResponseEntity<>(obj, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/packages/{id}")
    public ResponseEntity getConanPacakgeFile(@RepositoryMapping Repository repository,
                                              @PathVariable("name") String name,
                                              @PathVariable("version") String version,
                                              @PathVariable("user") String user,
                                              @PathVariable("channel") String channel,
                                              @PathVariable("id") String id) throws IOException {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        String packagePath = String.format("%s/%s/%s/%s/0/package/%s", user, name, version, channel, id);
        logger.info("Requested getConanPacakgeFile /{}/{}/{}.", storageId, repositoryId, packagePath);
        List<String> fileListName = Arrays.asList("conaninfo.txt", "conan_package.tgz", "conanmanifest.txt");

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), packagePath);
        if (!Files.exists(repositoryPath)) {
            fileListName.forEach(file -> {
                try {
                    String filePath = packagePath + "/0/" + file;
                    artifactResolutionService.resolvePath(storageId, repositoryId, filePath);
                } catch (Exception e) {
                    logger.error(" fetchPath error {}", e.getMessage());
                }
            });
            repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), packagePath);
            if (!Files.exists(repositoryPath)) {
                return new ResponseEntity<>("Not found package id", HttpStatus.NOT_FOUND);
            }

        }
        Map<String, Object> resultMap = new HashMap<>();
        fileListName.forEach(x -> {
            String path = packagePath + "/0/" + x;
            Artifact artifact = repositoryPathResolver.findOneArtifact(storageId, repositoryId, path);
            resultMap.put(x, artifact.getChecksums().get("MD5"));
        });
        return new ResponseEntity<>(resultMap, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/packages/{id}/download_urls")
    public ResponseEntity getPackageFileUrl(
            @RepositoryMapping Repository repository,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("user") String user,
            @PathVariable("channel") String channel,
            @PathVariable("id") String id) {
        Map<String, Object> map = new HashMap<String, Object>();
        String url = getBaseUrl(repository);
        List<String> list = Lists.newArrayList("conaninfo.txt", "conan_package.tgz", "conanmanifest.txt");
        list.forEach(x -> {
            map.put(x, String.format("%s/v1/files/%s/%s/%s/%s/0/package/%s/0/%s", url, user, name, version, channel, id, x));
        });
        return new ResponseEntity<>(map, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @PostMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/packages/{id}/upload_urls")
    public ResponseEntity uploadPackagesUrls(@RepositoryMapping Repository repository,
                                             @PathVariable("name") String name,
                                             @PathVariable("version") String version,
                                             @PathVariable("user") String user,
                                             @PathVariable("channel") String channel,
                                             @PathVariable("id") String id,
                                             @RequestBody(required = false) LinkedHashMap<String, String> obj) {
        String url = getBaseUrl(repository);
        obj.entrySet().forEach(entry -> {
            String packageName = entry.getKey();
            entry.setValue(String.format("%s/v1/files/%s/%s/%s/%s/0/package/%s/0/%s", url, user, name, version, channel, id, packageName));
        });
        return new ResponseEntity<>(obj, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = "{storageId}/{repositoryId}/v1/files/{path:.+}",
            method = {RequestMethod.GET})
    public void getConanFile(@RepositoryMapping Repository repository,
                             @PathVariable("path") String path,
                             HttpServletResponse response) throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested getConanFile /{}/{}/{}.", storageId, repositoryId, path);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        if (repositoryPath == null && path.endsWith("/conan_export.tgz")) {
            path = path.replaceAll("(?<=.*?/)conan_export.tgz$", "conan_sources.tgz");
            repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        }
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return;
        }
        vulnerabilityBlock(repositoryPath);
        try (InputStream in = Files.newInputStream(repositoryPath);) {
            OutputStream out = response.getOutputStream();
            response.setCharacterEncoding("UTF-8");
            // 设置文件头：设置下载文件名
            response.setHeader("Content-Disposition", "attachment;" + repositoryPath.getFileName().toString());
            int byteRead = 0;
            byte[] buffer = new byte[1024];
            while ((byteRead = in.read(buffer)) != -1) {
                out.write(buffer, 0, byteRead);
            }
            out.flush();
            artifactEventListenerRegistry.dispatchArtifactDownloadingEvent(repositoryPath);
        } catch (Exception e) {
            logger.error("download conan artifact error {}", ExceptionUtils.getStackTrace(e));
        }
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = "{storageId}/{repositoryId}/v1/files/{path:.+}",
            method = {RequestMethod.PUT})
    public ResponseEntity uploadFiles(@RepositoryMapping Repository repository,
                                      @PathVariable("path") String path,
                                      @RequestBody(required = false) byte[] is) throws IOException {
        if (is == null) {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND.getReasonPhrase(), HttpStatus.NOT_FOUND);
        }
        try (InputStream inputStream = new ByteArrayInputStream(is)) {
            final String storageId = repository.getStorage().getId();
            final String repositoryId = repository.getId();
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            logger.info("conan upload path {}", repositoryPath.toString());
            artifactManagementService.validateAndStore(repositoryPath, inputStream);
            return ResponseEntity.status(HttpStatus.CREATED).body("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/download_urls"})
    public ResponseEntity downloadUrls(@RepositoryMapping Repository repository,
                                       @PathVariable("name") String name,
                                       @PathVariable("version") String version,
                                       @PathVariable("user") String user,
                                       @PathVariable("channel") String channel) throws Exception {
        return conanArtifactServer.downloadUrls(repository, name, version, user, channel);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/v1/conans/{name}/{version}/{user}/{channel}/search"})
    public ResponseEntity search(@RepositoryMapping Repository repository,
                                 @PathVariable("name") String name,
                                 @PathVariable("version") String version,
                                 @PathVariable("user") String user,
                                 @PathVariable("channel") String channel) {
        return conanArtifactServer.searchConanPackageInfo(repository, name, version, user, channel);
    }

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
            Map<String, String> requiresMap = getPackageInfo(conanInfoString, "[full_requires]");
            conanPackageInfo = ConanPackageInfo.builder().settings(settingsMap).options(optionsMap).requires(requiresMap).build();
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
        String requiresKey = "[full_requires]";
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
                if (requiresKey.equalsIgnoreCase(key)) {
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
