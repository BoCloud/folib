package com.veadan.folib.controllers;

import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.controllers.support.ErrorResponseEntityBody;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.utils.ArtifactControllerHelper;
import com.veadan.folib.web.Constants;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMethod;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.util.Objects;

public abstract class BaseArtifactController
        extends BaseController {

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Inject
    private ArtifactRepository artifactRepository;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private HttpServletResponse httpServletResponse;

    @Autowired
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Autowired
    private ArtifactComponent artifactComponent;


    protected boolean provideArtifactDownloadResponse(HttpServletRequest request,
                                                      HttpServletResponse response,
                                                      HttpHeaders httpHeaders,
                                                      RepositoryPath repositoryPath)
            throws Exception {
        logger.info("Resolved path: {}", repositoryPath);
        boolean isCommitted = response.isCommitted();
        if (isCommitted) {
            return false;
        }
        ArtifactControllerHelper.provideArtifactHeaders(response, repositoryPath);
        if (response.getStatus() == HttpStatus.NOT_FOUND.value()) {
            return false;
        } else if (request.getMethod().equals(RequestMethod.HEAD.name())) {
            return true;
        }


        try (InputStream is = artifactResolutionService.getInputStream(repositoryPath)) {
            if (ArtifactControllerHelper.isRangedRequest(httpHeaders)) {
                logger.info("Detected ranged request.");

                ArtifactControllerHelper.handlePartialDownload(is, httpHeaders, response);
            } else {
                copyToResponse(is, response);
            }
        }

        return true;
    }

    public ResponseEntity<String> checkRepositoryAccess() {
        return new ResponseEntity<>("success", HttpStatus.OK);
    }

    /**
     * 漏洞阻断下载
     *
     * @param repositoryPath 制品信息
     * @throws IOException io异常
     */
    public void vulnerabilityBlock(RepositoryPath repositoryPath) throws IOException {
        boolean supportLayout = artifactComponent.layoutSupportsForBlock(repositoryPath);
        if (!supportLayout) {
            return;
        }
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (Objects.nonNull(artifact)) {
            boolean block = artifactComponent.vulnerabilityBlock(artifact);
            if (block) {
                httpServletResponse.setContentType(org.springframework.http.MediaType.APPLICATION_JSON_VALUE);
                httpServletResponse.setStatus(HttpServletResponse.SC_FORBIDDEN);
                String msg = "The artifact " + artifact.getUuid() + " has a vulnerability, and downloading is prohibited";
                httpServletResponse.getWriter().println(objectMapper.writeValueAsString(new ErrorResponseEntityBody(msg)));
                httpServletResponse.flushBuffer();
                artifactEventListenerRegistry.dispatchArtifactDownloadBlockedEvent(repositoryPath);
            }
        }
    }

    protected String getBaseUrl() {
        return StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/");
    }

    protected String getBaseUrl(Repository repository) {
        return String.format("%s/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }
}
