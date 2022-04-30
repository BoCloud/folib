package com.veadan.folib.controllers;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.utils.ArtifactControllerHelper;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.InputStream;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMethod;

public abstract class BaseArtifactController
        extends BaseController
{

    @Inject
    protected ArtifactManagementService artifactManagementService;

    protected boolean provideArtifactDownloadResponse(HttpServletRequest request,
                                                      HttpServletResponse response,
                                                      HttpHeaders httpHeaders,
                                                      RepositoryPath repositoryPath)
            throws Exception
    {
        logger.debug("Resolved path: {}", repositoryPath);
        ArtifactControllerHelper.provideArtifactHeaders(response, repositoryPath);
        if (response.getStatus() == HttpStatus.NOT_FOUND.value())
        {
            return false;
        }
        else if (request.getMethod().equals(RequestMethod.HEAD.name()))
        {
            return true;
        }

        
        try (InputStream is = artifactResolutionService.getInputStream(repositoryPath))
        {
            if (ArtifactControllerHelper.isRangedRequest(httpHeaders))
            {
                logger.debug("Detected ranged request.");

                ArtifactControllerHelper.handlePartialDownload(is, httpHeaders, response);
            }
            else
            {
                copyToResponse(is, response);
            }
        }

        return true;
    }

    public ResponseEntity<String> checkRepositoryAccess()
    {
        return new ResponseEntity<>("success", HttpStatus.OK);
    }

}
