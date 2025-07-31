package com.folib.controllers.layout.nuget;

import com.folib.artifact.coordinates.NugetCoordinates;
import com.folib.controllers.BaseArtifactController;
import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nuget.indexer.NugetMetadataExtractor;
import com.folib.nuget.indexer.model.NuSpecPackage;
import com.folib.nuget.indexer.model.NugetMetadata;
import com.folib.nuget.indexer.symbols.NugetSymbolsIndexer;
import com.folib.nuget.odata.feed.Entry;
import com.folib.nuget.odata.feed.Feed;
import com.folib.nuget.odata.utils.NuGetMetadataProvider;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.service.NugetCacheService;
import com.folib.service.NugetServiceFactory;
import com.folib.service.NugetV2Service;
import com.folib.web.LayoutReqMapping;
import com.folib.storage.repository.Repository;

import com.folib.web.RepoMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.JAXBException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;


@RestController
@LayoutReqMapping(NugetCoordinates.LAYOUT_NAME)
@Api(description = "Nuget_v2坐标控制器", tags = "Nuget_v2坐标控制器")
public class NugetArtifactV2Controller extends BaseArtifactController {

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private NugetCacheService nugetCacheService;

    @Inject
    private NugetServiceFactory nugetServiceFactory;


    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}/")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }


    @ApiOperation(value = "NugetV2-获取服务文档")
    @GetMapping(path = "{storageId}/{repositoryId}/api/v2",
            produces = MediaType.APPLICATION_XML)
    public ResponseEntity getNugetV2(@RepoMapping Repository repository) {
        String serviceDocument = NuGetMetadataProvider.getServiceDocumentEntity(repository);
        return ResponseEntity.ok(serviceDocument);

    }


    @ApiOperation(value = "NugetV2-获取元数据模板")
    @GetMapping(path = "{storageId}/{repositoryId}/api/v2/$metadata",
            produces = MediaType.APPLICATION_XML)
    public ResponseEntity getNugetV2Metadata(@RepoMapping Repository repository,
                                             HttpServletResponse response) throws IOException, JAXBException {
        String metadataTemplate = NuGetMetadataProvider.getMetadataTemplate(repository);
        return ResponseEntity.ok(metadataTemplate);
    }


    @ApiOperation(value = "推送包")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = {"{storageId}/{repositoryId}/api/v2/package/",
            "{storageId}/{repositoryId}/api/v2"},
            method = {RequestMethod.PUT, RequestMethod.POST},
            consumes = MediaType.MULTIPART_FORM_DATA)
    public ResponseEntity uploadPackage(@RequestHeader(name = "X-NuGet-ApiKey", required = true) String apiKey,
                                        @RepoMapping Repository repository,
                                        @RequestParam(value = "package") MultipartFile file) {
        boolean file_exists = false;
        RepositoryPath artifactPath = null;
        try {
            // 1. 提取nuspec文件
            NugetMetadataExtractor nugetMetadataExtractor = new NugetMetadataExtractor();
            NuSpecPackage nuSpecPackage = nugetMetadataExtractor.extractNuspecFromStream(file.getInputStream());
            // 2. 获取nuspec文件中的metadata
            NugetMetadata metadata = nuSpecPackage.getMetadata();
            // 3. 获取nuspec文件中的id和version
            String id = metadata.getId();
            String version = metadata.getVersion();
            // 4. 构造坐标和路径
            NugetCoordinates coordinates = new NugetCoordinates(id, version);
            artifactPath = repositoryPathResolver.resolve(repository, coordinates);
            if (artifactPath != null && Files.exists(artifactPath)) {
                file_exists = true;
            }
            // 5. 存储包
            InputStream is = file.getInputStream();
            artifactManagementService.validateAndStore(artifactPath, is);
            // 6. v2, v3索引
            nugetCacheService.cachePackage(repository, metadata);
        } catch (Exception e) {
            revertCommit(artifactPath, file_exists);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Failed to store file, " + e.getMessage());
        }
        return ResponseEntity.ok("Artifact uploaded successfully");
    }


    @ApiOperation(value = "推送符号包")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "{storageId}/{repositoryId}/api/v2/symbols/",
            method = {RequestMethod.PUT, RequestMethod.POST},
            consumes = MediaType.MULTIPART_FORM_DATA)
    public ResponseEntity uploadSymbolPackage(@RequestHeader(name = "X-NuGet-ApiKey", required = true) String apiKey,
                                              @RepoMapping Repository repository,
                                              @RequestParam(value = "package") MultipartFile file) {
        boolean file_exists = false;
        RepositoryPath artifactPath = null;
        try {
            // 1. 提取nuspec文件
            NugetMetadataExtractor nugetMetadataExtractor = new NugetMetadataExtractor();
            NuSpecPackage nuSpecPackage = nugetMetadataExtractor.extractNuspecFromStream(file.getInputStream());
            // 2. 获取nuspec文件中的metadata
            NugetMetadata metadata = nuSpecPackage.getMetadata();
            // 3. 获取nuspec文件中的id和version
            String id = metadata.getId();
            String version = metadata.getVersion();
            // 4. 构造坐标和路径
            NugetCoordinates coordinates = new NugetCoordinates(id, version, NugetCoordinates.SYMBOL_EXTENSION);
            artifactPath = repositoryPathResolver.resolve(repository, coordinates);
            if (artifactPath != null && Files.exists(artifactPath)) {
                file_exists = true;
            }
            // 需保证标准包存在
            RepositoryPath standardArtifactPath = repositoryPathResolver.resolve(repository, new NugetCoordinates(id, version));
            if (standardArtifactPath == null || !Files.exists(standardArtifactPath)) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("Standard package does not exist");
            }
            // 5. 存储符号包
            InputStream is = file.getInputStream();
            artifactManagementService.validateAndStore(artifactPath, is);
            // 6. 存储符号文件
            NugetSymbolsIndexer nugetSymbolsIndexer = new NugetSymbolsIndexer(repositoryPathResolver);
            nugetSymbolsIndexer.indexSymbolArtifact(artifactPath);
            // 7. v2, v3索引 TODO
        } catch (Exception e) {
            revertCommit(artifactPath, file_exists);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Failed to store file, " + e.getMessage());
        }
        return ResponseEntity.ok("Artifact uploaded successfully");
    }

    /**
     * 删除包
     * 1. 清除索引
     * 2. 清除目录 .symbols/id.version
     * 3. 清除目录 id/version, 如果id下没有其他版本, 则删除id目录
     *
     * @param apiKey
     * @param repository
     * @return
     */
    @ApiOperation(value = "删除包")
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    @DeleteMapping(path = "{storageId}/{repositoryId}/api/v2/package/{packageId}/{version}")
    public ResponseEntity deletePackage(@RequestHeader(name = "X-NuGet-ApiKey", required = true) String apiKey,
                                        @RepoMapping Repository repository,
                                        @PathVariable(value = "packageId") String packageId,
                                        @PathVariable(value = "version") String version) {
        packageId = toLowerId(packageId);
        try {
            // 1. 清除目录 .symbols/id.version
            RepositoryPath symbolsDirPath = this.getSymbolDir(repository, packageId, version);
            if (Files.exists(symbolsDirPath)) {
                artifactManagementService.delete(symbolsDirPath, true);
            }
            // 2. 清除目录 id/version, 如果id下没有其他版本, 则删除id目录
            RepositoryPath packageDirPath = this.getPackageDir(repository, packageId, version);
            if (Files.exists(packageDirPath)) {
                artifactManagementService.delete(packageDirPath, true);
            }
            RepositoryPath parentDirPath = packageDirPath.getParent();
            // 如果为空目录, 则删除
            if (parentDirPath != null && Files.exists(parentDirPath) && Files.list(parentDirPath).count() == 0) {
                artifactManagementService.delete(parentDirPath, true);
            }
            // 3. 清除索引(v2, v3)
            nugetCacheService.deCachePackage(repository, packageId, version);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Failed to delete package, " + e.getMessage());
        }
        return ResponseEntity.ok("Artifact deleted successfully");
    }


    @ApiOperation(value = "下载包内容")
    @GetMapping(path = "{storageId}/{repositoryId}/api/v2/download/nupkg/{packageId}/{version}")
    public void getNupkg(@RepoMapping Repository repository,
                         @PathVariable(value = "packageId") String packageId,
                         @PathVariable(value = "version") String version,
                         @RequestHeader HttpHeaders httpHeaders,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        // 1. 转换为小写
        packageId = toLowerId(packageId);
        NugetV2Service nugetV2Service = nugetServiceFactory.getNugetV2Service(repository);
        nugetV2Service.provideDownloadNupkg(repository, packageId, version, response);
    }


    @ApiOperation(value = "下载符号包snupkg")
    @GetMapping(path = "{storageId}/{repositoryId}/api/v2/symbols/{packageId}/{version}")
    public void getSnupkg(@RepoMapping Repository repository,
                          @PathVariable(value = "packageId") String packageId,
                          @PathVariable(value = "version") String version,
                          @RequestHeader HttpHeaders httpHeaders,
                          HttpServletRequest request,
                          HttpServletResponse response)
            throws Exception {
        packageId = toLowerId(packageId);
        NugetV2Service nugetV2Service = nugetServiceFactory.getNugetV2Service(repository);
        nugetV2Service.provideDownloadSnupkg(repository, packageId, version, response);
    }


    @ApiOperation("Search packages in repository")
    @GetMapping(path = {"{storageId}/{repositoryId}/api/v2/Search()"},
            produces = MediaType.APPLICATION_XML)
    public ResponseEntity searchPackages(@RepoMapping Repository repository,
                                         @RequestParam(name = "searchTerm", required = false) String searchTerm,
                                         @RequestParam(name = "includePrerelease", required = false, defaultValue = "false") boolean includePrerelease,
                                         @RequestParam(value = "skip", required = false, defaultValue = "0") int skip,
                                         @RequestParam(value = "top", required = false, defaultValue = "1000") int top,
                                         @RequestParam(value = "semVerLevel", required = false, defaultValue = "2.0.0") String semVerLevel,
                                         HttpServletResponse response) throws IOException, JAXBException {

        boolean shouldRemoveSemver2 = semVerLevel != null && !semVerLevel.equals("2.0.0");
        searchTerm = toLowerId(searchTerm);
        NugetSearchRequest searchRequest = NugetSearchRequest.builder()
                .repository(repository)
                .searchTerm(searchTerm)
                .includePreRelease(includePrerelease)
                .skip(skip)
                .take(top)
                .shouldRemoveSemver2(shouldRemoveSemver2)
                .build();
        try {
            NugetV2Service nugetV2Service = nugetServiceFactory.getNugetV2Service(repository);
            Feed feed = nugetV2Service.search(searchRequest);
            if (feed == null) {
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body("No packages found");
            }
            String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
            feed.rewrite(v2BaseUrl);
            // 设置响应头
            response.setContentType(MediaType.APPLICATION_XML);
            feed.writeXml(response.getOutputStream());

            return ResponseEntity.ok().build();
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Failed to search packages: " + e.getMessage());
        }
    }


    @ApiOperation("Get package entry")
    @GetMapping(path = "{storageId}/{repositoryId}/api/v2/Packages(Id='{packageId}',Version='{version}')",
            produces = MediaType.APPLICATION_XML)
    public ResponseEntity getPackageEntry(@RepoMapping Repository repository,
                                          @PathVariable("packageId") String packageId,
                                          @PathVariable("version") String version,
                                          HttpServletResponse response) throws IOException, JAXBException {
        packageId = toLowerId(packageId);
        try {
            NugetV2Service nugetV2Service = nugetServiceFactory.getNugetV2Service(repository);
            Entry entry = nugetV2Service.packageEntry(repository, packageId, version);
            if (entry == null) {
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body("Package not found");
            }
            String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
            entry.rewrite(v2BaseUrl);
            // 设置响应头
            response.setContentType(MediaType.APPLICATION_XML);
            entry.writeXml(response.getOutputStream());
            return ResponseEntity.ok().build();
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Failed to get package entry: " + e.getMessage());
        }
    }


    @ApiOperation("Find Packages by Id")
    @GetMapping(path = "{storageId}/{repositoryId}/api/v2/FindPackagesById()",
            produces = MediaType.APPLICATION_XML)
    public ResponseEntity findPackagesById(@RepoMapping Repository repository,
                                           @RequestParam("id") String id,
                                           @RequestParam("semVerLevel") String semVerLevel,
                                           HttpServletResponse response) throws IOException, JAXBException {
        id = toLowerId(id);
        boolean isSemVer2Endpoint = semVerLevel != null && !semVerLevel.equals("2.0.0");
        try {
            NugetV2Service nugetV2Service = nugetServiceFactory.getNugetV2Service(repository);
            Feed feed = nugetV2Service.findPackageById(repository, id, isSemVer2Endpoint);
            if (feed == null) {
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body("No packages found for id: " + id);
            }
            String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
            feed.rewrite(v2BaseUrl);
            // 设置响应头
            response.setContentType(MediaType.APPLICATION_XML);
            feed.writeXml(response.getOutputStream());
            return ResponseEntity.ok().build();
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Failed to find packages by id: " + e.getMessage());
        }
    }

    private void revertCommit(RepositoryPath artifactPath, boolean fileExistsOriginally) {
        // 1. 如果文件原先存在, 则不删除
        if (fileExistsOriginally) {
            return;
        }
        try {
            artifactManagementService.delete(artifactPath, true);
        } catch (IOException e) {
            logger.error("Failed to delete artifact path: {}", artifactPath, e);
        }
    }

    private String toLowerId(String id) {
        if (id == null) {
            return null;
        }
        while (id.length() > 1 && id.startsWith("'") && id.endsWith("'")) {
            id = id.substring(1, id.length() - 1);
        }
        return id.toLowerCase();
    }

    private RepositoryPath getSymbolDir(Repository repository, String id, String version) {
        String symbolDir = String.join("/", ".symbols", id + "." + version).toLowerCase();
        return repositoryPathResolver.resolve(repository, symbolDir);
    }

    private RepositoryPath getPackageDir(Repository repository, String id, String version) {
        String packageDir = String.join("/", id, version);
        return repositoryPathResolver.resolve(repository, packageDir);
    }


}
