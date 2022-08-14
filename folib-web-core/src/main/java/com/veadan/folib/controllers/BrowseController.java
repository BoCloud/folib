package com.veadan.folib.controllers;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.XmlUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.beust.jcommander.internal.Lists;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.dependency.snippet.CodeSnippet;
import com.veadan.folib.dependency.snippet.SnippetGenerator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.FileContent;
import com.veadan.folib.io.StorageFileSystem;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.DirectoryListingService;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.utils.TreeUtil;
import com.veadan.folib.web.RepositoryMapping;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.*;
import java.util.stream.Collectors;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;
import org.w3c.dom.Document;

import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

/**
 * REST API for browsing storage/repository/filesystem structures.
 *
 * @author Guido Grazioli <guido.grazioli@gmail.com>
 */
@RestController
@RequestMapping(path = BrowseController.ROOT_CONTEXT)
public class BrowseController
        extends BaseController {

    private static final Logger logger = LoggerFactory.getLogger(BrowseController.class);

    // must be the same as @RequestMapping value on the class definition
    public final static String ROOT_CONTEXT = "/api/browse";
    @Inject
    protected ArtifactManagementService artifactManagementService;
    @Inject
    private SnippetGenerator snippetGenerator;

    @Inject
    private PropertiesBooter propertiesBooter;


    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;


    //    @PreAuthorize("authenticated")
    @GetMapping(value = "/getArtifact/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity getArtifact(@PathVariable String artifactPath,
                                      @PathVariable String storageId,
                                      @PathVariable String repositoryId,
                                      @RequestParam("type") String type) {
        JSONObject jsonObject = new JSONObject();
        if (!type.equals("docker")) {
            Artifact artifact = repositoryPathResolver.findOneArtifact(storageId, repositoryId, artifactPath);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            Repository repository = repositoryPath.getRepository();

            if (artifact != null) {
                List<CodeSnippet> snippets = snippetGenerator.generateSnippets(repository.getLayout(), artifact.getArtifactCoordinates());
                jsonObject.put("snippets", snippets);
            }

            if (artifact != null) {
                TreeUtil treeUtil = new TreeUtil();
                SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                if (artifact.getCreated() != null) {
                    String createdTime = DateUtil.format(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                    jsonObject.put("createdTime", createdTime);
                }

                if (artifact.getLastUsed() != null) {
                    String lastUsedTime = DateUtil.format(Date.from(artifact.getLastUsed().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                    jsonObject.put("lastUsedTime", lastUsedTime);
                }

                Set<String> fileNames = artifact.getArtifactArchiveListing().getFilenames();

                if (fileNames != null && fileNames.size() > 0) {
                    List listTree = treeUtil.toTree(fileNames);
                    jsonObject.put("listTree", listTree);
                }

                jsonObject.put("downloadCount", artifact.getDownloadCount());
                jsonObject.put("sha", artifact.getChecksums().get("SHA-1"));
                jsonObject.put("md5", artifact.getChecksums().get("MD5"));
                jsonObject.put("artifact", artifact);
            }
        } else {
            String[] a = artifactPath.split("/");
            String aName = a[0];
            String aVersion = a[1];
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            String blobsPath=aName+"/blobs";
            RepositoryPath repositoryPathBlobs = repositoryPathResolver.resolve(storageId, repositoryId, blobsPath);
            try {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);

                //获取blobs下的文件列表，为了获取层的大小。
                DirectoryListing blobsListing = directoryListingService.fromRepositoryPath(repositoryPathBlobs);



                List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> !(file.getName().endsWith(".sha256"))).collect(Collectors.toList());  //+propertiesBooter.getStorageBooterBasedir()+"/"+propertiesBooter.getVaultDirectory() + "/storages/"
                FileContent fileContent = fileContents.get(0);

                String menifestString = FileUtil.readString(repositoryPath.toFile().getPath()+"/"+fileContent.getName(), "UTF-8");

                String iamgeName = configurationManagementService.getConfiguration().getBaseUrl().replace("http://", "") + storageId + "/" + repositoryId + "/" + aName + ":" + aVersion;
                String code = "docker  pull  " + iamgeName;
                CodeSnippet codeSnippet = new CodeSnippet("Docker", code);
                List<CodeSnippet> snippets = new ArrayList<>();
                snippets.add(codeSnippet);
                ImageManifest menifest = JSON.parseObject(menifestString, ImageManifest.class);

                List<String> digestList = menifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
                List<FileContent> fileblobs= Optional.ofNullable(blobsListing.getFiles()).orElse(Lists.newArrayList()).stream().filter(file ->digestList.contains(file.getName())).collect(Collectors.toList());
                String configDigest= menifest.getConfig().getDigest();
                String imagePath=repositoryPath.toFile().getPath().substring(0,repositoryPath.toFile().getPath().lastIndexOf("/"));
                String manifestConfigString=FileUtil.readString(imagePath+"/blobs/"+configDigest,"UTF-8");

                Long size =fileblobs.stream().mapToLong(FileContent::getSize).sum();

                jsonObject.put("sha256", menifest.getConfig().getDigest());
                jsonObject.put("snippets", snippets);
                jsonObject.put("manifest", menifest);
                JSONObject object = JSON.parseObject(manifestConfigString);
                jsonObject.put("manifestConfig",object);

                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                dateFormat.setTimeZone(TimeZone.getTimeZone("Asia/Shanghai"));
                String format = dateFormat.format(fileContent.getLastModified());
                jsonObject.put("lastModified",format);
                jsonObject.put("size",size);
                jsonObject.put("imageName",iamgeName);

            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        return ResponseEntity.status(OK)
                .body(jsonObject);
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @ApiOperation(value = "List the contents for a docker.")
    @GetMapping(value = "/getDockerArtifact/{storageId}/{repositoryId}/{path}")
    public Object getDockerArtifact(@PathVariable("storageId") String storageId,
                                    @PathVariable("repositoryId") String repositoryId,
                                    @PathVariable(value = "path", required = false) String path) {
        JSONObject jsonObject = new JSONObject();
        if (path == null) {

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            try {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                List<FileContent> imageDirList = directoryListing.getDirectories();
                jsonObject.put("directories", imageDirList);
                jsonObject.put("files", new JSONArray());
            } catch (IOException e) {
                jsonObject.put("directories", new JSONArray());
                jsonObject.put("message", "获取失败");
            }
            return ResponseEntity.status(OK)
                    .body(jsonObject);
        } else {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            try {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                List<FileContent> imageDirList = directoryListing.getDirectories().stream().filter(f -> (!f.getName().equals("blobs")) && (!f.getName().equals("manifest"))).collect(Collectors.toList());
                jsonObject.put("files", imageDirList);
                jsonObject.put("directories", new JSONArray());
            } catch (IOException e) {
                jsonObject.put("files", new JSONArray());
                jsonObject.put("message", "获取失败");
            }

        }
        return jsonObject;
    }

    @ApiOperation(value = "List configured storages.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The list was returned."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(produces = {MediaType.TEXT_PLAIN_VALUE,
            MediaType.TEXT_HTML_VALUE,
            MediaType.APPLICATION_JSON_VALUE})
    public Object storages(ModelMap model,
                           HttpServletRequest request,
                           @RequestHeader(value = HttpHeaders.ACCEPT, required = false) String acceptHeader) {
        logger.debug("Requested browsing for storages");

        try {
            Map<String, Storage> storages = configurationManager.getConfiguration().getStorages();
            DirectoryListing directoryListing = directoryListingService.fromStorages(storages);

            if (acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE)) {
                return ResponseEntity.ok(objectMapper.writer().writeValueAsString(directoryListing));
            }

            model.addAttribute("showBack", false);
            model.addAttribute("currentUrl", StringUtils.chomp(request.getRequestURI(), "/"));
            model.addAttribute("directories", directoryListing.getDirectories());
            model.addAttribute("files", directoryListing.getFiles());

            return new ModelAndView("directoryListing", model);
        } catch (Exception e) {
            String message = "Attempt to browse storages failed. Check server logs for more information.";
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, message, e, acceptHeader);
        }
    }

    @ApiOperation(value = "List configured repositories for a storage.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The list was returned."),
            @ApiResponse(code = 404, message = "The requested storage was not found."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "/{storageId}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.TEXT_HTML_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public Object repositories(@ApiParam(value = "The storageId", required = true) @PathVariable("storageId") String storageId,
                               HttpServletRequest request,
                               ModelMap model,
                               @RequestHeader(value = HttpHeaders.ACCEPT, required = false) String acceptHeader) {
        logger.debug("Requested browsing for repositories in storage : {}", storageId);

        try {
            Storage storage = configurationManager.getConfiguration().getStorage(storageId);
            if (storage == null) {
                return getNotFoundResponseEntity("The requested storage was not found.", acceptHeader);
            }

            DirectoryListing directoryListing = directoryListingService.fromRepositories(storage.getRepositories());

            if (acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE)) {
                return ResponseEntity.ok(objectMapper.writer().writeValueAsString(directoryListing));
            }

            model.addAttribute("currentUrl", StringUtils.chomp(request.getRequestURI(), "/"));
            model.addAttribute("directories", directoryListing.getDirectories());
            model.addAttribute("files", directoryListing.getFiles());

            return new ModelAndView("directoryListing", model);
        } catch (Exception e) {
            String message = "Attempt to browse repositories failed. Check server logs for more information.";
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, message, e, acceptHeader);
        }
    }

    @ApiOperation(value = "Deletes a path from a repository.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The artifact was deleted."),
            @ApiResponse(code = 400, message = "Bad request."),
            @ApiResponse(code = 404, message = "The specified storageId/repositoryId/path does not exist!") })
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity delete(@RepositoryMapping Repository repository,
                                 @ApiParam(value = "Whether to use force delete")
                                 @RequestParam(defaultValue = "false",
                                         name = "force",
                                         required = false) boolean force,
                                 @PathVariable String artifactPath)
            throws IOException
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Deleting {}:{}/{}...", storageId, repositoryId, artifactPath);

        try
        {
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (!Files.exists(repositoryPath))
            {
                return ResponseEntity.status(NOT_FOUND)
                        .body("The specified path does not exist!");
            }

            artifactManagementService.delete(repositoryPath, force);
        }
        catch (ArtifactStorageException e)
        {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(e.getMessage());
        }

        return ResponseEntity.ok("The artifact was deleted.");
    }

    @ApiOperation(value = "List the contents for a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The list was returned."),
            @ApiResponse(code = 404, message = "The requested storage, repository, or path was not found."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/{path:.+}"},
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.TEXT_HTML_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public Object repositoryContent(@RepositoryMapping Repository repository,
                                    @PathVariable("path") String rawPath,
                                    HttpServletRequest request,
                                    ModelMap model,
                                    @RequestHeader(value = HttpHeaders.ACCEPT, required = false) String acceptHeader) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.debug("Requested browsing repository content at {}/{}/{} ", storageId, repositoryId, rawPath);
        try {
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, rawPath);
            if (repositoryPath == null || !Files.exists(repositoryPath)) {
                return getNotFoundResponseEntity("The requested repository path was not found.", acceptHeader);
            }

            if (!repository.isInService()) {
                return getServiceUnavailableResponseEntity("Repository is not in service...", acceptHeader);
            }

            if (!repository.allowsDirectoryBrowsing() || !probeForDirectoryListing(repositoryPath)) {
                return getNotFoundResponseEntity("Requested repository doesn't allow browsing.", acceptHeader);
            }

            DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);

            if (acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE)) {
                return ResponseEntity.ok(objectMapper.writer().writeValueAsString(directoryListing));
            }
            URL resourceUrl = RepositoryFiles.readResourceUrl(repositoryPath);
            String downloadBaseUrl = StringUtils.chomp(resourceUrl.toString(), "/");
            String currentUrl = StringUtils.chomp(request.getRequestURI(), "/");
            model.addAttribute("currentUrl", currentUrl);
            model.addAttribute("downloadBaseUrl", downloadBaseUrl);
            model.addAttribute("directories", directoryListing.getDirectories());
            model.addAttribute("files", directoryListing.getFiles());

            return new ModelAndView("directoryListing", model);
        } catch (Exception e) {
            String message = "Failed to generate repository directory listing.";
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, message, e, acceptHeader);
        }
    }

    protected boolean probeForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException {
        return Files.exists(repositoryPath) &&
                Files.isDirectory(repositoryPath) &&
                isPermittedForDirectoryListing(repositoryPath);
    }

    protected boolean isPermittedForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException {
        //TODO: RepositoryFiles.isIndex(repositoryPath) || (
        return !Files.isHidden(repositoryPath) && !RepositoryFiles.isTrash(repositoryPath)
                && !RepositoryFiles.isTemp(repositoryPath);
    }

}
