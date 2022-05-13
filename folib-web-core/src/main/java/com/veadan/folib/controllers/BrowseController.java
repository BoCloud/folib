package com.veadan.folib.controllers;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.XmlUtil;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.services.DirectoryListingService;
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
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.*;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;
import org.w3c.dom.Document;

import static org.springframework.http.HttpStatus.OK;

/**
 * REST API for browsing storage/repository/filesystem structures.
 *
 * @author Guido Grazioli <guido.grazioli@gmail.com>
 */
@RestController
@RequestMapping(path = BrowseController.ROOT_CONTEXT)
public class BrowseController
        extends BaseController
{

    private static final Logger logger = LoggerFactory.getLogger(BrowseController.class);

    // must be the same as @RequestMapping value on the class definition
    public final static String ROOT_CONTEXT = "/api/browse";

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    //    @PreAuthorize("authenticated")
    @GetMapping(value = "/getArtifact/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity getArtifact(@RequestHeader HttpHeaders httpHeaders,
                                      @PathVariable String artifactPath,
                                      @PathVariable String storageId,
                                      @PathVariable String repositoryId,
                                      @RequestParam("type") String type,
                                      HttpServletRequest request,
                                      HttpServletResponse response)
    {
        Artifact artifact= repositoryPathResolver.findOneArtifact(storageId,repositoryId,artifactPath);
        JSONObject jsonObject = new JSONObject();
        if(artifact!=null) {

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
            if(type.equals("maven")){
                MavenArtifactCoordinates artifactCoordinates = (MavenArtifactCoordinates) artifact.getArtifactCoordinates();
                String mavenStr = null;
                String gradleStr = null;
                String ivyStr = null;
                String sbtStr = null;
                if (artifactCoordinates != null && artifactCoordinates.getExtension().equals("jar")) {
                    Set<String> fileNames = artifact.getArtifactArchiveListing().getFilenames();
                    List listTree = treeUtil.toTree(fileNames);

                    mavenStr =
                            "<dependency>\n" +
                                    "    <groupId>" + artifactCoordinates.getGroupId() + "</groupId>\n" +
                                    "    <artifactId>" + artifactCoordinates.getArtifactId() + "</artifactId>\n" +
                                    "    <version>" + artifactCoordinates.getVersion() + "</version>\n" +
                                    "</dependency>";

//            mavenStr= XmlUtil.toStr(XmlUtil.parseXml(mavenStr),true);
                    gradleStr = "compile(group: '" + artifactCoordinates.getGroupId() + "', name: '" + artifactCoordinates.getArtifactId() + "', version: '" + artifactCoordinates.getVersion() + "')";
                    ivyStr = "<dependency org=\"" + artifactCoordinates.getGroupId() + "\" name=\"" + artifactCoordinates.getArtifactId() + "\" rev=\"" + artifactCoordinates.getVersion() + "\">\n" +
                            "    <artifact name=\"" + artifactCoordinates.getArtifactId() + "\" ext=\"" + artifactCoordinates.getExtension() + "\"/>\n" +
                            "</dependency>";
//            ivyStr= XmlUtil.toStr(XmlUtil.parseXml(ivyStr),true);
                    sbtStr = "libraryDependencies += \"" + artifactCoordinates.getGroupId() + "\" % \"" + artifactCoordinates.getArtifactId() + "\" % \"" + artifactCoordinates.getVersion() + "\"";
                    jsonObject.put("mavenStr", mavenStr);
                    jsonObject.put("gradleStr", gradleStr);
                    jsonObject.put("ivyStr", ivyStr);
                    jsonObject.put("sbtStr", sbtStr);
                    jsonObject.put("listTree", listTree);
                }
            }else if(type.equals("npm")){
                NpmArtifactCoordinates artifactCoordinates = (NpmArtifactCoordinates) artifact.getArtifactCoordinates();
            }



            jsonObject.put("downloadCount", artifact.getDownloadCount());
            jsonObject.put("sha", artifact.getChecksums().get("SHA-1"));
            jsonObject.put("md5", artifact.getChecksums().get("MD5"));
            jsonObject.put("artifact", artifact);
        }

        return ResponseEntity.status(OK)
                .body(jsonObject);
    }


    @ApiOperation(value = "List configured storages.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The list was returned."),
                            @ApiResponse(code = 500, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.TEXT_HTML_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public Object storages(ModelMap model,
                           HttpServletRequest request,
                           @RequestHeader(value = HttpHeaders.ACCEPT, required = false) String acceptHeader)
    {
        logger.debug("Requested browsing for storages");
        
        try
        {
            Map<String, Storage> storages = configurationManager.getConfiguration().getStorages();
            DirectoryListing directoryListing = directoryListingService.fromStorages(storages);

            if (acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE))
            {
                return ResponseEntity.ok(objectMapper.writer().writeValueAsString(directoryListing));
            }

            model.addAttribute("showBack", false);
            model.addAttribute("currentUrl", StringUtils.chomp(request.getRequestURI(), "/"));
            model.addAttribute("directories", directoryListing.getDirectories());
            model.addAttribute("files", directoryListing.getFiles());

            return new ModelAndView("directoryListing", model);
        }
        catch (Exception e)
        {
            String message = "Attempt to browse storages failed. Check server logs for more information.";
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, message, e, acceptHeader);
        }
    }

    @ApiOperation(value = "List configured repositories for a storage.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The list was returned."),
                            @ApiResponse(code = 404, message = "The requested storage was not found."),
                            @ApiResponse(code = 500, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value="/{storageId}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.TEXT_HTML_VALUE,
                             MediaType.APPLICATION_JSON_VALUE})
    public Object repositories(@ApiParam(value = "The storageId", required = true) @PathVariable("storageId") String storageId,
                               HttpServletRequest request,
                               ModelMap model,
                               @RequestHeader(value = HttpHeaders.ACCEPT, required = false) String acceptHeader)
    {
        logger.debug("Requested browsing for repositories in storage : {}", storageId);

        try
        {
            Storage storage = configurationManager.getConfiguration().getStorage(storageId);
            if (storage == null)
            {
                return getNotFoundResponseEntity("The requested storage was not found.", acceptHeader);
            }

            DirectoryListing directoryListing = directoryListingService.fromRepositories(storage.getRepositories());

            if (acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE))
            {
                return ResponseEntity.ok(objectMapper.writer().writeValueAsString(directoryListing));
            }

            model.addAttribute("currentUrl", StringUtils.chomp(request.getRequestURI(), "/"));
            model.addAttribute("directories", directoryListing.getDirectories());
            model.addAttribute("files", directoryListing.getFiles());

            return new ModelAndView("directoryListing", model);
        }
        catch (Exception e)
        {
            String message = "Attempt to browse repositories failed. Check server logs for more information.";
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, message, e, acceptHeader);
        }
    }

    @ApiOperation(value = "List the contents for a repository.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The list was returned."),
                            @ApiResponse(code = 404, message = "The requested storage, repository, or path was not found."),
                            @ApiResponse(code = 500, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = { "{storageId}/{repositoryId}/{path:.+}" },
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.TEXT_HTML_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public Object repositoryContent(@RepositoryMapping Repository repository,
                                    @PathVariable("path") String rawPath,
                                    HttpServletRequest request,
                                    ModelMap model,
                                    @RequestHeader(value = HttpHeaders.ACCEPT, required = false) String acceptHeader)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.debug("Requested browsing repository content at {}/{}/{} ", storageId, repositoryId, rawPath);

        try
        {
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, rawPath);
            if (repositoryPath == null || !Files.exists(repositoryPath))
            {
                return getNotFoundResponseEntity("The requested repository path was not found.", acceptHeader);
            }

            if (!repository.isInService())
            {
                return getServiceUnavailableResponseEntity("Repository is not in service...", acceptHeader);
            }

            if (!repository.allowsDirectoryBrowsing() || !probeForDirectoryListing(repositoryPath))
            {
                return getNotFoundResponseEntity("Requested repository doesn't allow browsing.", acceptHeader);
            }

            DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);

            if (acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE))
            {
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
        }
        catch (Exception e)
        {
            String message = "Failed to generate repository directory listing.";
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, message, e, acceptHeader);
        }
    }

    protected boolean probeForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException
    {
        return Files.exists(repositoryPath) &&
               Files.isDirectory(repositoryPath) &&
               isPermittedForDirectoryListing(repositoryPath);
    }

    protected boolean isPermittedForDirectoryListing(final RepositoryPath repositoryPath)
            throws IOException
    {
        //TODO: RepositoryFiles.isIndex(repositoryPath) || (
        return !Files.isHidden(repositoryPath) && !RepositoryFiles.isTrash(repositoryPath)
                && !RepositoryFiles.isTemp(repositoryPath);
    }

}
