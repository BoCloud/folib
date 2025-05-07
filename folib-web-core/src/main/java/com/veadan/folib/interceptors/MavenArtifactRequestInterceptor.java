package com.veadan.folib.interceptors;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.artifact.MavenArtifact;
import com.veadan.folib.artifact.MavenArtifactUtils;
import com.veadan.folib.storage.repository.Repository;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.index.context.IndexingContext;
import org.springframework.web.servlet.HandlerMapping;
import static com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates.LAYOUT_NAME;
import static com.veadan.folib.storage.metadata.MetadataHelper.*;
import static org.springframework.http.HttpStatus.BAD_REQUEST;

/**
 * @author veadan
 */
public class MavenArtifactRequestInterceptor
        extends ArtifactRequestInterceptor
{

    private final RepositoryPathResolver repositoryPathResolver;

    public MavenArtifactRequestInterceptor(RepositoryPathResolver repositoryPathResolver)
    {
        super(LAYOUT_NAME);
        this.repositoryPathResolver = repositoryPathResolver;
    }

    @Override
    protected boolean preHandle(Repository repository,
                                String artifactPath,
                                HttpServletRequest request,
                                HttpServletResponse response)
            throws IOException
    {
        final Map pathVariables = (Map) request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
        if (pathVariables == null)
        {
            return true;
        }

        if (artifactPath.endsWith("/"))
        {
            response.sendError(BAD_REQUEST.value(), "The specified path should not ends with `/` character!");
            return false;
        }

        final boolean mavenArtifactRequest = isMavenArtifactRequest(request.getMethod());
        if (!mavenArtifactRequest)
        {
            return true;
        }
        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, artifactPath);
        if (Files.exists(repositoryPath) && Files.isDirectory(repositoryPath))
        {
            response.sendError(BAD_REQUEST.value(), "The specified path is a directory!");
            return false;
        }
        final String filename = repositoryPath.getFileName().toString();
        if (MAVEN_METADATA_XML.equals(filename) ||
            MAVEN_METADATA_XML_CHECKSUM_MD5.equals(filename) ||
            MAVEN_METADATA_XML_CHECKSUM_SHA1.equals(filename) ||
            MAVEN_METADATA_XML_CHECKSUM_SHA256.equals(filename) ||
            filename.startsWith(IndexingContext.INDEX_FILE_PREFIX))
        {
            return true;
        }
        boolean isValidGavPath = MavenArtifactUtils.isGAV(repositoryPath);
        if (!isValidGavPath)
        {
            response.sendError(BAD_REQUEST.value(), "The specified path is invalid. Maven GAV not recognized.");
            return false;
        }
        final MavenArtifact mavenArtifact = MavenArtifactUtils.convertPathToArtifact(repositoryPath);
        if (StringUtils.isBlank(mavenArtifact.getArtifactId()))
        {
            response.sendError(BAD_REQUEST.value(),
                               "The specified path is invalid. Maven artifact artifactId not recognized.");
            return false;
        }
        if (StringUtils.isBlank(mavenArtifact.getGroupId()))
        {
            response.sendError(BAD_REQUEST.value(),
                               "The specified path is invalid. Maven artifact groupId not recognized.");
            return false;
        }
        if (StringUtils.isBlank(mavenArtifact.getVersion()))
        {
            response.sendError(BAD_REQUEST.value(),
                               "The specified path is invalid. Maven artifact version not recognized.");
            return false;
        }
        if (StringUtils.isBlank(mavenArtifact.getType()))
        {
            response.sendError(BAD_REQUEST.value(),
                               "The specified path is invalid. Maven artifact type not recognized.");
            return false;
        }

        return true;
    }

    private static boolean isMavenArtifactRequest(final String method)
    {
        return "get".equalsIgnoreCase(method) ||
               "head".equalsIgnoreCase(method) ||
               "post".equalsIgnoreCase(method) ||
               "put".equalsIgnoreCase(method) ||
               "patch".equalsIgnoreCase(method);
    }

}
