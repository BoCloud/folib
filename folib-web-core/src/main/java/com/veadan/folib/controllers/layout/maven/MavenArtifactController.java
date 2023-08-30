package com.veadan.folib.controllers.layout.maven;

import com.alibaba.fastjson.JSONArray;
import com.google.common.collect.Sets;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.*;
import org.eclipse.jetty.http.ResourceHttpContent;
import org.eclipse.jetty.server.HttpOutput;
import org.eclipse.jetty.util.resource.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

import static org.springframework.http.HttpStatus.NOT_FOUND;

/**
 * REST API for all artifact-related processes.
 * <p>
 * Thanks to custom URL processing any path variable like '{artifactPath:.+}' will be processed as '**'.
 *
 * @author Martin Todorov
 * @author
 * @author veadan
 * @author @author veadan
 * @see {@linkplain http://docs.spring.io/spring/docs/current/spring-framework-reference/html/mvc.html#mvc-config-path-matching}
 */
@RestController
@LayoutRequestMapping(MavenArtifactCoordinates.LAYOUT_NAME)
//@RequestMapping(
//        headers = "user-agent=Maven/*")
@Api(description = "maven坐标控制器", tags = "maven坐标控制器")

public class MavenArtifactController
        extends BaseArtifactController {

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "Requested path not found."),
            @ApiResponse(code = 500, message = "Server error."),
            @ApiResponse(code = 503, message = "Repository currently not in service.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/{storageId}/{repositoryId}/{artifactPath:.+}"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void download(@RepositoryMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable String artifactPath,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
//        final String storageId = repository.getStorage().getId();
//        final String repositoryId = repository.getId();
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, artifactPath);
        artifactPath = correctIndexPathIfNecessary(repository, artifactPath);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
//        Class<?> headerWriterResponseClass = response.getOutputStream().getClass();
//        Field delegateField = headerWriterResponseClass.getDeclaredField("delegate");
//        delegateField.setAccessible(true);
//        Object delegate = delegateField.get(response.getOutputStream());
//        Resource resource = Resource.newResource(repositoryPath);
//        ResourceHttpContent resourceHttpContent = new ResourceHttpContent(resource, null, 0);
//        response.setHeader(HttpHeaders.CONTENT_LENGTH, Long.toString(resource.length()));
//        ((HttpOutput) delegate).sendContent(resourceHttpContent);
//        logger.info("耗时 {}ms", System.currentTimeMillis() - startTime);
//        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
//        try (InputStream is = Files.newInputStream(repositoryPath)) {
//            try (OutputStream os = response.getOutputStream()) {
//                int readLength;
//                byte[] bytes = new byte[4096];
//                while ((readLength = is.read(bytes)) != -1) {
//                    // Write the artifact
//                    os.write(bytes, 0, readLength);
//                }
//                os.flush();
//            }
//        }
//        Class<?> headerWriterResponseClass = response.getOutputStream().getClass();
//        Field delegateField = headerWriterResponseClass.getDeclaredField("delegate");
//        delegateField.setAccessible(true);
//        Object delegate = delegateField.get(response.getOutputStream());
//        Resource resource = Resource.newResource(repositoryPath);
//        ResourceHttpContent resourceHttpContent = new ResourceHttpContent(resource , null, 0);
//        ((HttpOutput) delegate).sendContent(resourceHttpContent);

//        StreamUtils.copy(Files.newInputStream(repositoryPath), response.getOutputStream());
//        Resource resource = Resource.newResource(repositoryPath);
//        ByteBuffer buffer = BufferUtil.toBuffer(resource, true);
//        FileChannel fileChannel = repositoryPath.getFileSystem().provider().newFileChannel(repositoryPath, Sets.newHashSet(StandardOpenOption.READ));
//        ((HttpOutput)delegate).sendContent(Files.newInputStream(repositoryPath), new Callback()
//        {
//            @Override
//            public void succeeded()
//            {
//
//            }
//
//            @Override
//            public void failed(Throwable x)
//            {
//
//            }
//
//            @Override
//            public InvocationType getInvocationType()
//            {
//                return InvocationType.NON_BLOCKING;
//            }
//
//            @Override
//            public String toString()
//            {
//                return "";
//            }
//        });
//        FileChannel fileChannel = repositoryPath.getFileSystem().provider().newFileChannel(repositoryPath, Sets.newHashSet(StandardOpenOption.READ));
//        WritableByteChannel responseChannel = Channels.newChannel(response.getOutputStream());
//        fileChannel.transferTo(0, fileChannel.size(), responseChannel);
//        fastChannelCopy(fileChannel, responseChannel);
//
        // Call the wrapped response for processing
//        getHandler().handle(request, responseWrapper);
//        FileChannel fileChannel = repositoryPath.getFileSystem().provider().newFileChannel(repositoryPath, Sets.newHashSet(StandardOpenOption.READ));
//        FileUrlResource resource = new FileUrlResource(repositoryPath.getTarget().toUri().toString());

//        long fileSize = fileChannel.size();
//        byte[] data = new byte[(int) fileSize];
//        int bufferSize = 8192;
//        ByteBuffer buffer = ByteBuffer.allocateDirect(bufferSize);
//                httpHeaders.setContentType(MediaType.APPLICATION_OCTET_STREAM);
//        response.setHeader("Content-Type", MediaType.APPLICATION_OCTET_STREAM.toString());
//        ByteBuffer buffer = BufferUtil.toBuffer(resource, true);
//        responseChannel.write(buffer);
//        buffer.get(data);
//        httpHeaders.setContentType(MediaType.APPLICATION_OCTET_STREAM);
//        httpHeaders.setContentDispositionFormData("attachment", FilenameUtils.getName(repositoryPath.getFileName().toString()));
//        return new ResponseEntity<Object>(resource, httpHeaders, HttpStatus.OK);
//        artifactPath = correctIndexPathIfNecessary(repository, artifactPath);
//        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
////        vulnerabilityBlock(repositoryPath);
//        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }

    /**
     * Copies data from one channel to another
     *
     * @param src  channel source
     * @param dest destination channel
     * @throws IOException input / output error
     */
    private static void fastChannelCopy(final ReadableByteChannel src,
                                        final WritableByteChannel dest)
            throws IOException {
        final ByteBuffer buffer = ByteBuffer.allocateDirect(16 * 1024);

        while (src.read(buffer) != -1) {
            buffer.flip();
            dest.write(buffer);
            buffer.compact();
        }

        buffer.flip();

        while (buffer.hasRemaining()) {
            dest.write(buffer);
        }
    }

    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity upload(@RepositoryMapping Repository repository,
                                 @PathVariable String artifactPath,
                                 HttpServletRequest request) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, request.getInputStream());

            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @ApiOperation(value = "Copies a path from one repository to another.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The path was copied successfully."),
            @ApiResponse(code = 400, message = "Bad request."),
            @ApiResponse(code = 404, message = "The source/destination storageId/repositoryId/path does not exist!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_COPY')")
    @PostMapping(value = "/copy/{path:.+}")
    public ResponseEntity copy(
            @RepositoryMapping(storageVariableName = "srcStorageId", repositoryVariableName = "srcRepositoryId")
                    Repository srcRepository,
            @RepositoryMapping(storageVariableName = "destStorageId", repositoryVariableName = "destRepositoryId")
                    Repository destRepository,
            @PathVariable String path) {
        final String srcStorageId = srcRepository.getStorage().getId();
        final String srcRepositoryId = srcRepository.getId();
        final String destStorageId = destRepository.getStorage().getId();
        final String destRepositoryId = destRepository.getId();

        logger.info("Copying {} from {}:{} to {}:{}...", path, srcStorageId, srcRepositoryId, destStorageId,
                destRepositoryId);

        try {
            final RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(srcRepository, path);
            if (!Files.exists(srcRepositoryPath)) {
                return ResponseEntity.status(NOT_FOUND)
                        .body("The source path does not exist!");
            }

            RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, path);
            RepositoryPath destPath = repositoryPathResolver.resolve(destRepository, path);

            artifactManagementService.copy(srcPath, destPath);
        } catch (ArtifactStorageException e) {
            logger.error("Unable to copy artifact due to ArtifactStorageException", e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(e.getMessage());
        } catch (Exception e) {
            logger.error("Unable to copy artifact", e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }

        return ResponseEntity.ok("The path was copied successfully.");
    }

    @ApiOperation(value = "Deletes a path from a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deleted."),
            @ApiResponse(code = 400, message = "Bad request."),
            @ApiResponse(code = 404, message = "The specified storageId/repositoryId/path does not exist!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity delete(@RepositoryMapping Repository repository,
                                 @ApiParam(value = "Whether to use force delete")
                                 @RequestParam(defaultValue = "false",
                                         name = "force",
                                         required = false) boolean force,
                                 @PathVariable String artifactPath)
            throws IOException {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Deleting {}:{}/{}...", storageId, repositoryId, artifactPath);

        try {
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (!Files.exists(repositoryPath)) {
                return ResponseEntity.status(NOT_FOUND)
                        .body("The specified path does not exist!");
            }

            artifactManagementService.delete(repositoryPath, force);
        } catch (ArtifactStorageException e) {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(e.getMessage());
        }

        return ResponseEntity.ok("The artifact was deleted.");
    }

    private String correctIndexPathIfNecessary(final Repository repository,
                                               final String requestedPath) {
        return new MavenRepositoryIndexPathTransformer(repository).apply(requestedPath);
    }

}
