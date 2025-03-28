package com.veadan.folib.controllers.layout.nuget;

import com.veadan.folib.artifact.coordinates.NugetArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.json.*;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.MediaType;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.net.HttpURLConnection;

/**
 * This Controller used to handle Nuget requests.(api v2)
 * @author LingengMa
 */

/**
 // GET /Packages
 // GET /Packages/$count
 // GET /Packages(Id=,Version=)
 // GET/POST /FindPackagesById()?id=
 // GET /Packages(Id=,Version=)/propertyName
 // GET/POST /Search()?searchTerm=&targetFramework=&includePrerelease=
 // GET /Search()/$count?searchTerm=&targetFramework=&includePrerelease=
 // GET/POST /GetUpdates()?packageIds=&versions=&includePrerelease=&includeAllVersions=&targetFrameworks=&versionConstraints=
 // /api/v2/GetUpdates()/$count?packageIds=&versions=&includePrerelease=&includeAllVersions=&targetFrameworks=&versionConstraints=
 /// GET/HEAD /Packages(Id=,Version=)/Download
 /// DELETE /id/version
 /// PUT /

 */

@RestController
@LayoutRequestMapping(NugetArtifactCoordinates.LAYOUT_NAME)
@Api(description = "Nuget_v2坐标控制器",tags = "Nuget_v2坐标控制器")
public class NugetArtifactV2Controller extends BaseArtifactController {

//    @ApiOperation(value = "获取OData元数据")
//    @GetMapping(path = { "{storageId}/{repositoryId}/$metadata" }, produces = MediaType.APPLICATION_XML)
//    public ResponseEntity<Resource> getMetadata()
//    {
//        return null;
//    }


}
