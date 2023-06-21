package com.veadan.folib.controllers.layout.conan;

import com.google.common.collect.ImmutableMap;
import com.veadan.folib.config.FolibPublicUtils;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.io.LazyInputStream;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ConanArtifactServer;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.RepositoryMapping;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.nio.file.Files;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

//@LayoutRequestMapping("conan")
@RestController
@Slf4j
public class ConanArtifactController extends BaseArtifactController {

    @Autowired
    private ConanArtifactServer conanArtifactServer;

    @Autowired
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/ping")
    public ResponseEntity ping(@RequestHeader HttpHeaders httpHeaders,
                               HttpServletRequest request, HttpServletResponse response) {
        return new ResponseEntity<>("ok", HttpStatus.OK);
    }
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{username}/{channel}/digest")
    public ResponseEntity exportDigest(@RepositoryMapping Repository repository,
                                       @PathVariable("name") String name,
                                       @PathVariable("version") String version,
                                       @PathVariable("username") String username,
                                       @PathVariable("channel") String channel) throws IOException {
        String path = String.format("%s/%s/%s/%s/export/conanmanifest.txt", name, version, username, channel);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), path);
        if (null == repositoryPath || !Files.exists(repositoryPath)) {
            return new ResponseEntity<>("Not found", HttpStatus.NOT_FOUND);
        }
        String conanManifestTxtUrl = FolibPublicUtils.getRepositoryWebServerUrl(repository) + "/v1/files/" + path;
        Map<String, String> map = Map.of("conanmanifest.txt", conanManifestTxtUrl);
        return new ResponseEntity<>(map, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{packageName}/{version}/_/_/packages/{packageId}/digest")
    public ResponseEntity packagesDigest(@RepositoryMapping Repository repository,
                                         @PathVariable("packageName") String packageName,
                                         @PathVariable("version") String version,
                                         @PathVariable("packageId") String packageId) throws IOException {
        String packagePath = "_/" + packageName + "/" + version + "/_/0/package/" + packageId + "/0/conanmanifest.txt";
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), packagePath);
        if (!Files.exists(repositoryPath)) {
            return new ResponseEntity<>("ok", HttpStatus.NOT_FOUND);
        }
        Map<String, Object> map = new HashMap<>();
        String conanManifestTxtUrl = FolibPublicUtils.getRepositoryWebServerUrl(repository)
                + "/v1/files/_/" + packageName + "/" + version + "/_/0/package/" + packageId + "/0/conanmanifest.txt";
        map.put("conanmanifest.txt", conanManifestTxtUrl);
        return new ResponseEntity<>(map, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/users/authenticate")
    public ResponseEntity userAuthenticate(@RequestHeader(value = HttpHeaders.AUTHORIZATION, required = false)
                                                   String authorization,
                                           @RequestHeader(HttpHeaders.ACCEPT) String accept,
                                           @RequestHeader HttpHeaders httpHeaders,
                                           HttpServletRequest request, HttpServletResponse response) {

        return new ResponseEntity<>("ok", HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/users/check_credentials")
    public ResponseEntity checkCredentials(@RequestHeader(HttpHeaders.ACCEPT) String accept,
                                           @RequestHeader HttpHeaders httpHeaders,
                                           HttpServletRequest request, HttpServletResponse response) {
        return new ResponseEntity<>("ok", HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{username}/{channel}")
    public ResponseEntity checkExport(@RepositoryMapping Repository repository,
                                      @PathVariable("name") String name,
                                      @PathVariable("version") String version,
                                      @PathVariable("username") String username,
                                      @PathVariable("channel") String channel) {
                    /*{
              "conan_export.tgz" : "1ba8d47e32782d8a4f57e5fd71f2c757",
              "conanmanifest.txt" : "3e1fb90002521dbdad5bc49b671a826f",
              "conanfile.py" : "738c6b5381ef8cdc2bfba3e96b173561"
            }
            */
        return new ResponseEntity<>("ok", HttpStatus.NOT_FOUND);
    }

    /*查询仓库 已有的conan包  */
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/search")
    public ResponseEntity search(
            @RepositoryMapping Repository repository,
            @RequestParam(value = "q", required = false) String query) throws IOException {
        return conanArtifactServer.searchConanPackage(repository, query);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @PostMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{username}/{channel}/upload_urls")
    public ResponseEntity uploadExportUrls(@RepositoryMapping Repository repository,
                                           @PathVariable("name") String name,
                                           @PathVariable("version") String version,
                                           @PathVariable("username") String username,
                                           @PathVariable("channel") String channel,
                                           @RequestBody(required = false) LinkedHashMap<String, String> obj) {
        if (obj == null) {
            return new ResponseEntity<>("", HttpStatus.NOT_FOUND);
        }

        String url = FolibPublicUtils.getRepositoryWebServerUrl(repository);
        obj.entrySet().forEach(entry -> {
            String packageName = entry.getKey();
            entry.setValue(String.format("%s/v1/files/%s/%s/%s/%s/export/%s", url, name, version, username, channel, packageName));
        });
        return new ResponseEntity<>(obj, HttpStatus.OK);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{username}/{channel}/packages/{id}")
    public ResponseEntity getConanPacakgeFile(@RepositoryMapping Repository repository,
                                              @PathVariable("name") String name,
                                              @PathVariable("version") String version,
                                              @PathVariable("username") String username,
                                              @PathVariable("channel") String channel,
                                              @PathVariable("id") String id) throws IOException {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        String packagePath = String.format("%s/%s/%s/%s/0/package/%s", name, version, username, channel, id);
//        String packagePath = "_/" + name + "/" + version + "/_/0/package/" + id;
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
    @GetMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{username}/{channel}/packages/{code}/download_urls")
    public ResponseEntity getPackageFileUrl(
            @RepositoryMapping Repository repository,
            @PathVariable("name") String name,
            @PathVariable("version") String version,
            @PathVariable("username") String username,
            @PathVariable("channel") String channel,
            @PathVariable("code") String code) {
//        Map<String, Object> map = new HashMap<String, Object>();
//        String url = FolibPublicUtils.getRepositoryWebServerUrl(repository);
//        List<String> list = Arrays.asList(new String[]{"conaninfo.txt", "conan_package.tgz", "conanmanifest.txt"});
//        list.forEach(x -> {
//            map.put(x, url + "/v1/files/_/" + packageName + "/" + version + "/_/0/package/" + id + "/0/" + x);
//        });
//        return new ResponseEntity<>(map, HttpStatus.OK);
        ImmutableMap<Object, Object> map = ImmutableMap.builder()
                .put("errors", List.of(Map.of("status", 404, "message", "Not Found")))
                .build();
        return new ResponseEntity<>(map, HttpStatus.NOT_FOUND);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @PostMapping(value = "{storageId}/{repositoryId}/v1/conans/{name}/{version}/{username}/{channel}/packages/{id}/upload_urls")
    public ResponseEntity uploadPackagesUrls(@RepositoryMapping Repository repository,
                                             @PathVariable("name") String name,
                                             @PathVariable("version") String version,
                                             @PathVariable("username") String username,
                                             @PathVariable("channel") String channel,
                                             @PathVariable("id") String id,
                                             @RequestBody(required = false) LinkedHashMap<String, String> obj) {
        String url = FolibPublicUtils.getRepositoryWebServerUrl(repository);
        obj.entrySet().forEach(entry -> {
            String packageName = entry.getKey();
            entry.setValue(String.format("%s/v1/files/%s/%s/%s/%s/0/package/%s/0/%s", url, name, version, username, channel, id, packageName));
        });
//        Map<String, Object> map = new HashMap<String, Object>();
//        String url = FolibPublicUtils.getRepositoryWebServerUrl(repository);
//        obj.forEach((x, y) -> {
//            map.put(x.toString(), url + "/v1/files/_/" + packageName + "/" + version + "/_/0/package/" + id + "/0/" + x);
//        });
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
        vulnerabilityBlock(repositoryPath);
        try (InputStream in = Files.newInputStream(repositoryPath);) {
            OutputStream out = response.getOutputStream();
            response.setCharacterEncoding("UTF-8");
            // 设置文件头：设置下载文件名
            response.setHeader("Content-Disposition", "attachment;" + repositoryPath.getFileName().toString());
            if (path.endsWith("/conan_export.tgz")) {
                conanExportTgzRewriteUrl(repository, path, in, out);
            } else {
                int byteRead = 0;
                byte[] buffer = new byte[1024];
                while ((byteRead = in.read(buffer)) != -1) {
                    out.write(buffer, 0, byteRead);
                }
                out.flush();
            }
            artifactEventListenerRegistry.dispatchArtifactDownloadingEvent(repositoryPath);
        } catch (Exception e) {
            logger.error("download conan artifact error {}", e.getMessage());
        }
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = "{storageId}/{repositoryId}/v1/files/{path:.+}",
            method = {RequestMethod.PUT})
    public ResponseEntity uploadFiles(@RepositoryMapping Repository repository,
                                      @PathVariable("path") String path,
                                      @RequestBody(required = false) byte[] is) throws IOException {
        if (is == null) {
            return new ResponseEntity<>("", HttpStatus.NOT_FOUND);
        }
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            InputStream inputStream = new ByteArrayInputStream(is);
            logger.info("conan upload path {}", repositoryPath.toString());
            artifactManagementService.store(repositoryPath, inputStream);
            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/v1/conans/{name}/{version}/{username}/{channel}/download_urls"})
    public ResponseEntity downloadUrls(@RepositoryMapping Repository repository,
                                       @PathVariable("name") String name,
                                       @PathVariable("version") String version,
                                       @PathVariable("username") String username,
                                       @PathVariable("channel") String channel) throws Exception {
        return conanArtifactServer.downloadUrls(repository, name, version, username, channel);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/v1/conans/{name}/{version}/{username}/{channel}/search"})
    public ResponseEntity search(@RepositoryMapping Repository repository,
                                 @PathVariable("name") String name,
                                 @PathVariable("version") String version,
                                 @PathVariable("username") String username,
                                 @PathVariable("channel") String channel) {
        return conanArtifactServer.searchConanPackageInfo(repository, name, version);

    }


    private void conanExportTgzRewriteUrl(Repository repository, String path, InputStream is, OutputStream os) throws IOException {
        //解析坐标
        //todo 后期提取成常量
        Pattern compile = Pattern.compile("^(?<name>.+?)/(?<version>.+?)/(?<username>.+?)/(?<channel>.+?)/export/.+$");
        Matcher matcher = compile.matcher(path);
        String name = "";
        String version = "";
        String username = "";
        String channel = "";
        if (matcher.matches()) {
            name = matcher.group("name");
            version = matcher.group("version");
            username = matcher.group("username");
            channel = matcher.group("channel");
        }
        //解析conan_export.tgz
        ((LazyInputStream)is).init();
        GzipCompressorInputStream gzip = new GzipCompressorInputStream(is);
        TarArchiveInputStream tar = new TarArchiveInputStream(gzip);
        TarArchiveEntry entry;
        if ((entry = tar.getNextTarEntry()) != null) {
            //读取conandata.yaml
            DumperOptions dumperOptions = new DumperOptions();
            dumperOptions.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
            Yaml yaml = new Yaml(dumperOptions);
            Map<String, Map<String, Map<String, Object>>> properties;
            properties = yaml.load(tar);
            //获取url
            String url = (String) properties.get("sources").get(version).get("url");
            String packageName = url.substring(url.lastIndexOf("/") + 1);
            //修改url
            String localUrl = FolibPublicUtils.getRepositoryWebServerUrl(repository);
            String redirectURL = String.format("%s/v1/files/%s/%s/%s/%s/package/%s", localUrl, name, version, username, channel, packageName);
            properties.get("sources").get(version).put("url", redirectURL);
            File tempFile = File.createTempFile("tmp", "yaml");
            FileWriter fileWriter = new FileWriter(tempFile);
            yaml.dump(properties, fileWriter);
            //重新打包
            try (BufferedOutputStream buffOut = new BufferedOutputStream(os);
                 GzipCompressorOutputStream gzOut = new GzipCompressorOutputStream(buffOut);
                 TarArchiveOutputStream tOut = new TarArchiveOutputStream(gzOut)) {
                TarArchiveEntry tarEntry = new TarArchiveEntry(tempFile, entry.getName());
                tOut.putArchiveEntry(tarEntry);
                Files.copy(tempFile.toPath(), tOut);
                tOut.closeArchiveEntry();
                tOut.finish();
            }
        }
    }

}
