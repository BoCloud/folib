package com.veadan.folib.controllers.layout.docker;

import com.google.cloud.tools.jib.api.Containerizer;
import com.google.cloud.tools.jib.api.Jib;
import com.google.cloud.tools.jib.api.RegistryImage;
import com.google.cloud.tools.jib.api.TarImage;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

@LayoutRequestMapping(DockerArtifactCoordinates.LAYOUT_NAME)
@RestController
@Api(description = "docker存储空间控制器", tags = "docker存储空间控制器")
public class DockerStorageController extends BaseArtifactController {

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "Requested path not found."),
            @ApiResponse(code = 500, message = "Server error."),
            @ApiResponse(code = 503, message = "Repository currently not in service.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/{storageId}/{repositoryId}/{artifactPath:.+}"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void download(@RepositoryMapping Repository repository,
                         @PathVariable String artifactPath,
                         @RequestHeader HttpHeaders httpHeaders, HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested get docker application file {}/{}/{}.", storageId, repositoryId, artifactPath);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        vulnerabilityBlock(repositoryPath);
        response.setCharacterEncoding("UTF-8");
        response.setHeader("Content-Disposition", "attachment;" + repositoryPath.getFileName().toString());
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }


    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "Requested path not found."),
            @ApiResponse(code = 500, message = "Server error."),
            @ApiResponse(code = 503, message = "Repository currently not in service.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/{storageId}/{repositoryId}/upload"}, method = {RequestMethod.POST})
    public ResponseEntity<?> upload(@RepositoryMapping Repository repository,
                                 @RequestParam("file") MultipartFile multipartFile,
                                 @RequestParam(value = "imageTag") String path)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested get docker application file {}/{}/{}.", storageId, repositoryId, path);

        String  baseUrl= getBaseUrl();
        String url = String.join("/", baseUrl,storageId, repositoryId, path);//String.format("%s/%s", baseUrl, path);

        final String prefix1 = "http://";
        final String prefix2 = "https://";
        String tag = url.replaceAll("^" + prefix1, "");
        if (url.contains(prefix1)) {
            tag = url.replaceAll("^" + prefix1, "");
        } else if (url.contains(prefix2)) {
            tag = url.replaceAll("^" + prefix2, "");
        }
       String TEMP_UPLOAD_DIR = "uploads";
        // 将文件保存到指定目录下
        String fileName = multipartFile.getOriginalFilename();
        Path tempDirectory = Files.createTempDirectory(TEMP_UPLOAD_DIR);
        Path localPath = Paths.get(String.join("/", tempDirectory.toString(), fileName));
        Files.copy(multipartFile.getInputStream(), localPath);
        Jib.from(TarImage.at(localPath))
                .containerize(Containerizer.to(
                        RegistryImage
                                .named(tag)
                                .addCredential("admin", "folib@v587")
                ).setAllowInsecureRegistries(true));

        Files.delete(localPath);
        Files.delete(tempDirectory);
        return ResponseEntity.ok().build();
    }

}
