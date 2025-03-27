package com.veadan.folib.controllers.layout.nuget;

import com.veadan.folib.artifact.coordinates.NugetArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.UserRepositoryPermission;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;


import javax.json.*;
import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import javax.ws.rs.core.MediaType;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Collections;
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
                modifiedResource.add("@id", "http://localhost:38080/storages/public-project/bacadadas/v3/" + id);
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
    @GetMapping(path = "{storageId}/{repositoryId}/v3/flatcontainer/{packageId}/index.json",
                produces = MediaType.APPLICATION_JSON)
    public ResponseEntity indexPackage(@RepositoryMapping Repository repository,
                                                 @PathVariable(name = "packageId") String packageId)
    {
        return null;
    }


    @ApiOperation(value = "PackageBaseAddress/3.0.0-下载包内容.nupkg")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/flatcontainer/{packageId}/{version}/{packageId}.{version}.nupkg",
                produces = MediaType.APPLICATION_OCTET_STREAM)
    public ResponseEntity<Resource> downloadPackage(@RepositoryMapping Repository repository,
                                                    @PathVariable(name = "packageId") String packageId,
                                                    @PathVariable(name = "version") String packageVersion)
    {
        return null;
    }


    @ApiOperation(value = "PackageBaseAddress/3.0.0-下载包清单.nuspec")
    @GetMapping(path = "{storageId}/{repositoryId}/v3/flatcontainer/{packageId}/{version}/{packageID}.nuspec",
                produces = MediaType.APPLICATION_XML)
    public ResponseEntity<Resource> downloadNuspec(@RepositoryMapping Repository repository,
                                                   @PathVariable(name = "packageId") String packageId,
                                                   @PathVariable(name = "version") String packageVersion)
    {
        return null;
    }


    /**
     * PackagePublish/2.0.0
     */
    @ApiOperation(value = "PackagePublish/2.0.0-推送包")
    @RequestMapping(path = "{storageId}/{repositoryId}/v3/package",
                    method = RequestMethod.PUT,
                    consumes = MediaType.MULTIPART_FORM_DATA)
    public ResponseEntity<Resource> pushPackage(@RequestHeader(name = "X-NuGet-ApiKey") String apiKey,
                                                @RepositoryMapping Repository repository,
                                                @RequestParam(name = "package")MultipartFile file,
                                                HttpServletRequest request)
    {
        return null;
    }


    @ApiOperation(value = "PackagePublish/2.0.0-删除包")
    @DeleteMapping(path = "{storageId}/{repositoryId}/v3/package/{packageId}/{version}")
    public ResponseEntity<Resource> deletePackage(@RequestHeader(name = "X-NuGet-ApiKey") String apiKey,
                                                  @RepositoryMapping Repository repository,
                                                  @PathVariable(name = "packageId") String packageId,
                                                  @PathVariable(name = "version") String version)
    {
        return null;
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



}
