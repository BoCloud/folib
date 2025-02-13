package com.veadan.folib.services.impl;

import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.veadan.folib.components.StorageClientComponent;
import com.veadan.folib.components.PypiBrowsePackageHtmlResponseBuilder;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.constants.PypiConstants;
import com.veadan.folib.domain.client.ResponseResult;
import com.veadan.folib.enums.PypiRepositoryTypeEnum;
import com.veadan.folib.enums.ResponseDataTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.pypi.PypiSearchResult;
import com.veadan.folib.services.PypiProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import com.veadan.folib.util.PypiUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.nio.file.Files;
import java.util.List;
import java.util.Objects;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class PypiProxyProvider implements PypiProvider {

    @Inject
    private PypiProviderRegistry pypiProviderRegistry;

    @Inject
    private StorageClientComponent clientComponent;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private PypiBrowsePackageHtmlResponseBuilder pypiBrowsePackageHtmlResponseBuilder;

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

    @PostConstruct
    @Override
    public void register() {
        pypiProviderRegistry.addProvider(PypiRepositoryTypeEnum.PYPI_PROXY.getType(), this);
        log.info("Registered pypi provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), PypiRepositoryTypeEnum.PYPI_PROXY.getType());
    }

    @Override
    public String packages(Repository repository, String packageName, String targetUrl) {
        String packageMetadataFilePath = PypiUtils.getRemotePackageIndexPath(packageName);
        RepositoryPath packageHtmlRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
        try {
            String htmlData = null;
            if (Objects.isNull(packageHtmlRepositoryPath) || !Files.exists(packageHtmlRepositoryPath) || RepositoryFiles.hasRefreshContent(packageHtmlRepositoryPath)) {
                if (RepositoryFiles.hasRefreshContent(packageHtmlRepositoryPath)) {
                    log.info("Pypi indexJsonRepositoryPath [{}] [{}] [{}] refresh content", packageHtmlRepositoryPath.getStorageId(), packageHtmlRepositoryPath.getRepositoryId(), packageName);
                }
                htmlData = commonUrlJSONData(repository, targetUrl);
                if (StringUtils.isBlank(htmlData)) {
                    if (Files.exists(packageHtmlRepositoryPath)) {
                        return Files.readString(packageHtmlRepositoryPath);
                    }
                    return null;
                }
                try {
                    String storageId = repository.getStorage().getId();
                    String repositoryId = repository.getId();
                    String packageTargetUrl = "";
                    RemoteRepository remoteRepository = repository.getRemoteRepository();
                    if (remoteRepository.getUrl().endsWith(GlobalConstants.SEPARATOR)) {
                        packageTargetUrl = String.format("%s%s", remoteRepository.getUrl(), packageName);
                    } else {
                        packageTargetUrl = String.format("%s/%s", remoteRepository.getUrl(), packageName);
                    }
                    String prefix = "";
                    if (packageTargetUrl.contains("/storages/")) {
                        prefix = packageTargetUrl.substring(packageTargetUrl.indexOf("/storages/"), packageTargetUrl.indexOf("/simple/"));
                        if (!prefix.endsWith(GlobalConstants.SEPARATOR)) {
                            prefix = prefix + GlobalConstants.SEPARATOR;
                        }
                    }
                    String finalPrefix = prefix;
                    Matcher matcher = PypiConstants.PACKAGE_NAME_PATTERN.matcher(htmlData);
                    String finalPackageTargetUrl = packageTargetUrl;
                    List<PypiSearchResult> pypiSearchResultList = matcher.results().map(matchResult -> handleVersion(storageId, repositoryId, finalPackageTargetUrl, finalPrefix, matchResult))
                            .filter(Objects::nonNull).collect(Collectors.toList());
                    Files.createDirectories(packageHtmlRepositoryPath.getParent());
                    Files.writeString(packageHtmlRepositoryPath, pypiBrowsePackageHtmlResponseBuilder.getProxyHtmlResponse(pypiSearchResultList));
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            }
            return Files.readString(packageHtmlRepositoryPath);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public String getLocalPackages(Repository repository, String packageName, String targetUrl) {
        String packageMetadataFilePath = PypiUtils.getRemotePackageIndexPath(packageName);
        RepositoryPath packageHtmlRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
        try {
            if (Objects.nonNull(packageHtmlRepositoryPath) && Files.exists(packageHtmlRepositoryPath) && !RepositoryFiles.hasRefreshContent(packageHtmlRepositoryPath)) {
                return Files.readString(packageHtmlRepositoryPath);
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    private PypiSearchResult handleVersion(String storageId, String repositoryId, String packageTargetUrl, String finalPrefix, MatchResult matchResult) {
        String artifactName = "", artifactUrl = "";
        try {
            artifactName = matchResult.group(2);
            artifactUrl = matchResult.group(1);
            if (StringUtils.isNotBlank(finalPrefix) && artifactUrl.contains(finalPrefix)) {
                artifactUrl = artifactUrl.replace(finalPrefix, "/../../");
            }
            String artifactPath = artifactUrl.substring(artifactUrl.indexOf("/packages/") + "/packages/".length());
            artifactUrl = PypiUtils.resolveUrl(packageTargetUrl, artifactUrl);
            return PypiSearchResult.builder().artifactName(artifactName).artifactPath(artifactPath).artifactUrl(artifactUrl).storageId(storageId).repositoryId(repositoryId).groupName(PypiArtifactCoordinates.parse(artifactName).getId()).build();
        } catch (Exception ex) {
            log.error("Pypi storageId [{}] repositoryId [{}] packageName [{}] parse error [{}]", storageId, repositoryId, artifactName, ExceptionUtils.getStackTrace(ex));
//            throw new RuntimeException(ex.getMessage());
        }
        return null;
    }

    private String commonUrlJSONData(Repository repository, String url) {
        String data = null;
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository)) {
            log.warn("Remote storageId [{}] repositoryId [{}] url [{}] is down.", repository.getStorage().getId(), repository.getId(), remoteRepository.getUrl());
            return null;
        }
        String prefixUrl = remoteRepository.getUrl();
        String suffixUrl = url;
        if (!suffixUrl.startsWith(GlobalConstants.SEPARATOR)) {
            suffixUrl = GlobalConstants.SEPARATOR + suffixUrl;
        }
        String targetUrl = StringUtils.removeEnd(prefixUrl, GlobalConstants.SEPARATOR) + suffixUrl;
        ResponseResult responseResult = clientComponent.doGet(repository.getStorage().getId(), repository.getId(), targetUrl);
        if (Objects.isNull(responseResult)) {
            return null;
        }
        if (HttpStatus.OK.value() == responseResult.getHttpStatus() && ResponseDataTypeEnum.STRING.equals(responseResult.getDataType()) && StringUtils.isNotBlank(responseResult.getData())) {
            data = responseResult.getData();
        }
        return data;
    }

}
