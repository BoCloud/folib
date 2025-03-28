package com.veadan.folib.controllers.layout.nuget;

import com.alibaba.excel.metadata.Head;
import com.veadan.folib.artifact.coordinates.NugetArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PathNupkg;
import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.UserRepositoryPermission;
import com.veadan.folib.nuget.NugetSearchRequest;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.repository.RepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProviderRegistry;
import com.veadan.folib.providers.repository.RepositorySearchRequest;
import com.veadan.folib.repository.NugetRepositoryFeatures;
import com.veadan.folib.services.ArtifactTagService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.metadata.nuget.Nupkg;
import com.veadan.folib.storage.metadata.nuget.Nuspec;
import com.veadan.folib.storage.metadata.nuget.TempNupkgFile;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.http.HttpHeaders;


import javax.inject.Inject;
import javax.json.*;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.ws.rs.core.MediaType;
import java.io.*;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * This Controller used to handle Nuget requests.(api v3)
 * @author LingengMa
 */

@RestController
@LayoutRequestMapping(NugetArtifactCoordinates.LAYOUT_NAME)
@Api(description = "Nuget_v3坐标控制器",tags = "Nuget_v3坐标控制器")
public class NugetArtifactV3Controller extends BaseArtifactController {

    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private NugetRepositoryFeatures.RepositorySearchEventListener repositorySearchEventListener;

    @Value("classpath:service-index/nuget/index.json")
    private Resource indexJson;

    @ApiOperation(value = "获取服务索引")
    @GetMapping(path = {"{storageId}/{repositoryId}/v3/index.json"},
                produces = MediaType.APPLICATION_JSON)
    public ResponseEntity index(@RepositoryMapping Repository repository,
                                            @PathVariable(name = "storageId") String storageId,
                                            @PathVariable(name = "repositoryId") String repositoryId)
    {
        // 读取index.json文件, 并修改资源路径
        try (InputStream is = indexJson.getInputStream())
        {
            JsonReader reader = Json.createReader(is);
            JsonObject jsonObject = reader.readObject();
            reader.close();

            JsonArray resources = jsonObject.getJsonArray("resources");
            JsonArrayBuilder modifiedResources = Json.createArrayBuilder();

            for (JsonObject resource : resources.getValuesAs(JsonObject.class))
            {
                JsonObjectBuilder modifiedResource = Json.createObjectBuilder(resource);
                String id = resource.getString("@id");
//                modifiedResource.add("@id", storageId + "/" + repositoryId + "/" + id);
                modifiedResource.add("@id", "http://10.30.20.56:38080/storages/public-project/bacadadas/v3/" + id);
                modifiedResources.add(modifiedResource);
            }

            JsonObjectBuilder modifiedJson = Json.createObjectBuilder(jsonObject);
            modifiedJson.add("resources", modifiedResources);

            StringWriter stringWriter = new StringWriter();
            JsonWriter writer = Json.createWriter(stringWriter);
            writer.writeObject(modifiedJson.build());
            writer.close();

            return ResponseEntity.ok(stringWriter.toString());
        } catch (IOException e)
        {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error reading index.json");
        }
    }


    /**
     * PackageBaseAddress/3.0.0
     */
    @ApiOperation(value = "PackageBaseAddress/3.0.0-枚举包版本")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "OK"),
                            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "Package not Found")})
    @GetMapping(path = "{storageId}/{repositoryId}/v3/flatcontainer/{packageId}/index.json",
                produces = MediaType.APPLICATION_JSON)
    public ResponseEntity indexPackage(@RepositoryMapping Repository repository,
                                                 @PathVariable(name = "packageId") String packageId)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        String normalisedPackageId = normaliseSearchTerm(packageId);

        NugetSearchRequest nugetSearchRequest = new NugetSearchRequest();
        nugetSearchRequest.setFilter(String.format("tolower(Id) eq '%s'", packageId));
        repositorySearchEventListener.setNugetSearchRequest(nugetSearchRequest);

        RepositoryProvider provider = repositoryProviderRegistry.getProvider(repository.getType());

        Paginator paginator = new Paginator();
        paginator.setProperty("artifactCoordinates.coordinates.version");

        RepositorySearchRequest predicate = new RepositorySearchRequest(normalisedPackageId, Collections.singleton("nupkg"));

        Collection<? extends Nupkg> files = searchNupkg(storageId, repositoryId, provider, paginator, predicate);

        // 如果没有找到包, 返回404
        if (files.isEmpty())
        {
            return ResponseEntity.notFound().build();
        }

        // 提取版本号
        Set<String> versions = files.stream()
                .map(f -> f.getVersion().toString())
                .collect(Collectors.toSet());

        // 转化为json
        JsonObjectBuilder jsonObjectBuilder = Json.createObjectBuilder();
        JsonArrayBuilder jsonArrayBuilder = Json.createArrayBuilder();
        versions.forEach(jsonArrayBuilder::add);
        jsonObjectBuilder.add("versions", jsonArrayBuilder);

        return ResponseEntity.ok(jsonObjectBuilder.build().toString());
    }


    @ApiOperation(value = "PackageBaseAddress/3.0.0-下载包内容.nupkg")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "OK"),
                            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "Package not Found")})
    @GetMapping(path = "{storageId}/{repositoryId}/v3/flatcontainer/{packageId}/{version}/{packageId}.{version}.nupkg",
                produces = MediaType.APPLICATION_OCTET_STREAM)
    public void downloadPackage(@RepositoryMapping Repository repository,
                                                    @PathVariable(name = "packageId") String packageId,
                                                    @PathVariable(name = "version") String packageVersion,
                                                    HttpServletResponse response,
                                                    HttpServletRequest request,
                                                    @RequestHeader HttpHeaders httpHeaders)
            throws Exception
    {
        getPackageInternal(repository, packageId, packageVersion, response, request, httpHeaders);
    }


    @ApiOperation(value = "PackageBaseAddress/3.0.0-下载包清单.nuspec")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "OK"),
            @ApiResponse(code = HttpURLConnection.HTTP_NOT_FOUND, message = "Package not Found")})
    @GetMapping(path = "{storageId}/{repositoryId}/v3/flatcontainer/{packageId}/{version}/{packageID}.nuspec",
                produces = MediaType.APPLICATION_XML)
    public void downloadNuspec(@RepositoryMapping Repository repository,
                                                   @PathVariable(name = "packageId") String packageId,
                                                   @PathVariable(name = "version") String version,
                                                   HttpServletResponse response,
                                                   HttpServletRequest request,
                                                   @RequestHeader HttpHeaders httpHeaders)
    {
        try
        {
            // 构建包坐标
            NugetArtifactCoordinates coordinates = new NugetArtifactCoordinates(packageId, version);
            
            // 获取包路径
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, coordinates);
            
            // 检查包是否存在
            if (!Files.exists(repositoryPath))
            {
                response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                return;
            }

            // 获取nuspec文件
            Path nuspecFile = repositoryPath.resolveSibling(packageId + ".nuspec");
            if (!Files.exists(nuspecFile))
            {
                response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                return;
            }

            // 设置响应头
            response.setHeader("Content-Type", MediaType.APPLICATION_XML);
            response.setHeader("Content-Disposition", "attachment; filename=\"" + packageId + ".nuspec\"");

            // 写入响应流
            try (InputStream is = Files.newInputStream(nuspecFile);
                 OutputStream os = response.getOutputStream())
            {
                byte[] buffer = new byte[4096];
                int bytesRead;
                while ((bytesRead = is.read(buffer)) != -1)
                {
                    os.write(buffer, 0, bytesRead);
                }
                os.flush();
            }
        }
        catch (Exception e)
        {
            logger.error("下载nuspec文件失败", e);
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }

    }


    /**
     * PackagePublish/2.0.0
     * TODO XML解析
     */
    @ApiOperation(value = "PackagePublish/2.0.0-推送包")
    @ApiResponses(value = { @ApiResponse(code = HttpURLConnection.HTTP_OK, message = "The package was deployed successfully."),
                            @ApiResponse(code = HttpURLConnection.HTTP_INTERNAL_ERROR, message = "An error occurred.") })
//    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(path = "{storageId}/{repositoryId}/v3/package/",
                    method = RequestMethod.PUT,
                    consumes = MediaType.MULTIPART_FORM_DATA)
    public ResponseEntity pushPackage(@RequestHeader(name = "X-NuGet-ApiKey", required = false) String apiKey,
                                      @RepositoryMapping Repository repository,
                                      @RequestParam(value = "package") MultipartFile file,
                                      HttpServletRequest request)
    {

        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        logger.info("Nuget push request: storageId-[{}]; repositoryId-[{}]", storageId, repositoryId);

        URI resourceUri;
        try
        {
            InputStream packagePartInputStream = file.getInputStream();
            resourceUri = storePackage(storageId, repositoryId, packagePartInputStream);
        }
        catch (Exception e)
        {
            logger.error("Failed to process Nuget push request: {}:{}", storageId, repositoryId, e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

        if (resourceUri == null)
        {
            // Return 501 status in case of empty package recieved.
            // For some reason nuget.exe sends empty package first.
            return ResponseEntity.status(HttpURLConnection.HTTP_BAD_REQUEST).build();
        }

        return ResponseEntity.created(resourceUri).build();

    }


    @ApiOperation(value = "PackagePublish/2.0.0-删除包")
    @DeleteMapping(path = "{storageId}/{repositoryId}/v3/package/{packageId}/{version}")
    public ResponseEntity deletePackage(@RequestHeader(name = "X-NuGet-ApiKey", required = false) String apiKey,
                                                  @RepositoryMapping Repository repository,
                                                  @PathVariable(name = "packageId") String packageId,
                                                  @PathVariable(name = "version") String version)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        logger.info("Nuget delete request: storageId-[{}]; repositoryId-[{}]; packageId-[{}]", storageId, repositoryId,
                packageId);

        RepositoryPath path = repositoryPathResolver.resolve(storageId, repositoryId, String.format("%s/%s/%s.nuspec", packageId, version, packageId));;
        try
        {
            //TODO: we should move associated files deletion into corresponding layout providers.
            artifactManagementService.delete(path, true);
            path = repositoryPathResolver.resolve(storageId, repositoryId, String.format("%s/%s/%s.%s.nupkg", packageId, version, packageId,version));
            artifactManagementService.delete(path, true);
            path = repositoryPathResolver.resolve(storageId, repositoryId, String.format("%s/%s/%s.%s.nupkg.sha512", packageId, version, packageId,version));
            artifactManagementService.delete(path, true);
        }
        catch (IOException e)
        {
            logger.error("Failed to process Nuget delete request: path-[{}]", path, e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

        return ResponseEntity.status(HttpStatus.OK).build();
    }


    @ApiOperation(value = "PackagePublish/2.0.0-重新列出包")
    @PostMapping(path = "{storageId}/{repositoryId}/v3/package/{packageId}/{packageVersion}")
    public ResponseEntity<Resource> relistPackage(@RepositoryMapping Repository repository,
                                                  @PathVariable(name = "packageId") String packageId,
                                                  @PathVariable(name = "packageVersion") String packageVersion)
    {
        return null;
    }


    /*
     *  SearchQueryService/3.5.0
     *  向后兼容 SearchQueryService, SearchQueryService/3.0.0-beta 和 SearchQueryService/3.0.0-rc
     * */
    @ApiOperation(value = "SearchQueryService/3.5.0-搜索包")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/search")
    public ResponseEntity<Resource> searchPackage(@RepositoryMapping Repository repository,
                                                  @RequestParam(name = "q") String query,
                                                  @RequestParam(name = "skip") Integer skip,
                                                  @RequestParam(name = "take") Integer take,
                                                  @RequestParam(name = "prerelease") Boolean prerelease,
                                                  @RequestParam(name = "semVerLevel") String semVerLevel,
                                                  @RequestParam(name = "packageType") String packageType)
    {
        return null;
    }


    /**
     * RegistrationsBaseUrl
     * 等同于 RegistrationsBaseUrl/3.0.0 和 RegistrationsBaseUrl/3.0.0-rc
     */
    @ApiOperation(value = "RegistrationsBaseUrl-注册索引")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/registration5-semver1/index.json")
    public ResponseEntity<Resource> registrationsIndex(@RepositoryMapping Repository repository)
    {
        return null;
    }

    @ApiOperation(value = "RegistrationsBaseUrl-注册页")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/registration5-semver1/{packageID}/index.json")
    public ResponseEntity<Resource> registrationsPage(@RepositoryMapping Repository repository,
                                                      @PathVariable(name = "packageID") String packageId)
    {
        return null;
    }

    @ApiOperation(value = "RegistrationsBaseUrl-注册叶")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/registration5-semver1/{packageID}/{version}.json")
    public ResponseEntity<Resource> registrationsLeaf(@RepositoryMapping Repository repository,
                                                      @PathVariable(name = "packageID") String packageId,
                                                      @PathVariable(name = "version") String version)
    {
        return null;
    }

    /**
     * RegistrationsBaseUrl/3.4.0
     */
    @ApiOperation(value = "RegistrationsBaseUrl/3.4.0-注册索引")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/registration5-gz-semver1/index.json")
    public ResponseEntity<Resource> registrationsIndex_340(@RepositoryMapping Repository repository)
    {
        return null;
    }

    @ApiOperation(value = "RegistrationsBaseUrl/3.4.0-注册页")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/registration5-gz-semver1/{packageID}/index.json")
    public ResponseEntity<Resource> registrationsPage_340(@RepositoryMapping Repository repository,
                                                      @PathVariable(name = "packageID") String packageId)
    {
        return null;
    }

    @ApiOperation(value = "RegistrationsBaseUrl/3.4.0-注册叶")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/registration5-gz-semver1/{packageID}/{version}.json")
    public ResponseEntity<Resource> registrationsLeafV340(@RepositoryMapping Repository repository,
                                                      @PathVariable(name = "packageID") String packageId,
                                                      @PathVariable(name = "version") String version)
    {
        return null;
    }

    /**
     * RegistrationsBaseUrl/3.6.0
     */
    @ApiOperation(value = "RegistrationsBaseUrl/3.6.0-注册索引")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/registration5-gz-semver2/index.json")
    public ResponseEntity<Resource> registrationsIndexV360(@RepositoryMapping Repository repository)
    {
        return null;
    }

    @ApiOperation(value = "RegistrationsBaseUrl/3.6.0-注册页")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/registration5-gz-semver2/{packageID}/index.json")
    public ResponseEntity<Resource> registrationsPageV360(@RepositoryMapping Repository repository,
                                                           @PathVariable(name = "packageID") String packageId)
    {
        return null;
    }

    @ApiOperation(value = "RegistrationsBaseUrl/3.6.0-注册叶")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/registration5-gz-semver2/{packageID}/{version}.json")
    public ResponseEntity<Resource> registrationsLeafV360(@RepositoryMapping Repository repository,
                                                           @PathVariable(name = "packageID") String packageId,
                                                           @PathVariable(name = "version") String version)
    {
        return null;
    }


    // 查找Nuget包
    private List<PathNupkg> searchNupkg(String storageId,
                                        String repositoryId,
                                        RepositoryProvider provider,
                                        Paginator paginator,
                                        RepositorySearchRequest predicate)
    {
        return provider.search(storageId, repositoryId, predicate, paginator)
                .stream()
                .map(p -> {
                    try
                    {
                        return new PathNupkg((RepositoryPath) p);
                    }
                    catch (Exception e)
                    {
                        logger.error("Failed to resolve Nuget package path [{}]", p, e);
                        return null;
                    }
                })
                .collect(Collectors.toList());
    }


    private String normaliseSearchTerm(String sourceValue)
    {
        if (sourceValue == null)
        {
            return null;
        }

        return sourceValue.replaceAll("['\"]", "");
    }

    private URI storePackage(String storageId,
                             String repositoryId,
                             InputStream is)
            throws Exception
    {
        try (TempNupkgFile nupkgFile = new TempNupkgFile(is))
        {
            Nuspec nuspec = nupkgFile.getNuspec();
            if (nuspec == null)
            {
                return null;
            }

            String nuspecId = nuspec.getId();

            SemanticVersion nuspecVersion = nuspec.getVersion();
            String path = String.format("%s/%s/%s.%s.nupkg",
                    nuspecId,
                    nuspecVersion,
                    nuspecId,
                    nuspecVersion);

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            artifactManagementService.validateAndStore(repositoryPath, nupkgFile.getStream());

            Path nuspecFile = Files.createTempFile(nuspec.getId(), "nuspec");
            try (OutputStream outputStream = new BufferedOutputStream(Files.newOutputStream(nuspecFile)))
            {
                nuspec.saveTo(outputStream);
            }
            path = String.format("%s/%s/%s.nuspec", nuspecId, nuspecVersion, nuspecId);
            repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            try (InputStream bis = new BufferedInputStream(Files.newInputStream(nuspecFile)))
            {
                artifactManagementService.validateAndStore(repositoryPath, bis);
            }

            Path hashFile = Files.createTempFile(String.format("%s.%s", nuspecId, nuspecVersion),
                    "nupkg.sha512");

            try (OutputStream bos = new BufferedOutputStream(Files.newOutputStream(hashFile)))
            {
                Writer writer = new OutputStreamWriter(bos);
                writer.write(nupkgFile.getHash());
                writer.flush();
                bos.flush();
            }

            path = String.format("%s/%s/%s.%s.nupkg.sha512",
                    nuspecId,
                    nuspecVersion,
                    nuspecId,
                    nuspecVersion);
            repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            try (InputStream bis = new BufferedInputStream(Files.newInputStream(hashFile)))
            {
                artifactManagementService.validateAndStore(repositoryPath, bis);
            }
        }

        return new URI("");
    }

    private void getPackageInternal(Repository repository,
                                    String packageId,
                                    String packageVersion,
                                    HttpServletResponse response,
                                    HttpServletRequest request,
                                    org.springframework.http.HttpHeaders httpHeaders)
            throws Exception
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested Nuget Package {},{}, {}, {}.", storageId, repositoryId, packageId, packageVersion);

        String fileName = String.format("%s.%s.nupkg", packageId, packageVersion);
        String path = String.format("%s/%s/%s", packageId, packageVersion, fileName);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        vulnerabilityBlock(repositoryPath);
        if (provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath))
        {
            response.setHeader("Content-Disposition", String.format("attachment; filename=\"%s\"", fileName));
        }
    }




}
