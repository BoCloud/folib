package com.veadan.folib.providers.layout;

import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.config.NpmLayoutProviderConfig;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.enums.NpmSubLayout;
import com.veadan.folib.npm.metadata.Dependency;
import com.veadan.folib.npm.metadata.Dist;
import com.veadan.folib.npm.metadata.PackageVersion;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactTagService;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.time.ZoneId;
import java.util.Date;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

/**
 * @author xuxinping
 */
@Component
public class NpmPackageSupplier implements Function<Path, NpmPackageDesc> {

    private static final Logger logger = LoggerFactory.getLogger(NpmPackageSupplier.class);

    @Inject
    private NpmLayoutProvider layoutProvider;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    @NpmLayoutProviderConfig.NpmObjectMapper
    private ObjectMapper npmJacksonMapper;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Override
    public NpmPackageDesc apply(Path path) {
        RepositoryPath repositoryPath = (RepositoryPath) path;

        NpmFileSystemProvider npmFileSystemProvider = (NpmFileSystemProvider) path.getFileSystem().provider();

        NpmArtifactCoordinates c;
        Artifact artifactEntry;
        try {
            c = (NpmArtifactCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
            artifactEntry = repositoryPath.getArtifactEntry();
        } catch (IOException e) {
            throw new UndeclaredThrowableException(e);
        }

        NpmPackageDesc npmPackageDesc = new NpmPackageDesc();
        Date releaseDate = Date.from(artifactEntry.getCreated().atZone(ZoneId.systemDefault()).toInstant());
        if (Objects.nonNull(artifactEntry.getLastUpdated())) {
            releaseDate = Date.from(artifactEntry.getLastUpdated().atZone(ZoneId.systemDefault()).toInstant());
        }
        npmPackageDesc.setReleaseDate(releaseDate);

        PackageVersion npmPackage = null;
        String packagePath = NpmSubLayout.OHPM.getValue().equals(repositoryPath.getRepository().getSubLayout()) ? NpmLayoutProvider.OHPM_PACKAGE_JSON_PATH : NpmLayoutProvider.DEFAULT_PACKAGE_JSON_PATH;
        byte[] packageJsonBytes = layoutProvider.getContentByFileName(repositoryPath, repositoryPath, packagePath);

        if (Objects.nonNull(packageJsonBytes)) {
            String packageJson = new String(packageJsonBytes, StandardCharsets.UTF_8);
            try {
                npmPackage = npmJacksonMapper.readValue(packageJson, PackageVersion.class);
            } catch (Exception ex) {
                logger.warn("Artifact packageVersion 转换异常 [{}]", artifactEntry.getUuid());
            }
        }
        if (Objects.isNull(npmPackage)) {
            npmPackage = new PackageVersion();
        }
        npmPackageDesc.setNpmPackage(npmPackage);

        npmPackage.setAdditionalProperty("_id", String.format("%s@%s", c.getId(), c.getVersion()));

        npmPackage.setName(c.getId());
        npmPackage.setVersion(c.getVersion());
        Dist dist = new Dist();
        npmPackage.setDist(dist);

        if (StringUtils.isNotBlank(artifactEntry.getDependencies())) {
            JSONObject dependenciesJson = JSONObject.parseObject(artifactEntry.getDependencies());
            Dependency dependency = new Dependency();
            String value = "";
            for (Map.Entry<String, Object> entry : dependenciesJson.entrySet()) {
                if (Objects.nonNull(entry.getValue())) {
                    value = entry.getValue().toString();
                }
                dependency.setAdditionalProperty(entry.getKey(), value);
            }
            npmPackage.setDependencies(dependency);
        }

        Map<String, RepositoryPath> checksumMap = npmFileSystemProvider.resolveChecksumPathMap(repositoryPath);
        fetchShasum(dist, checksumMap);

        String url;
        try {
            URI uri = c.convertToLayoutResource(c);
            url = getRepositoryBaseUrl(repositoryPath.getRepository()) + uri.toString();
        } catch (Exception e) {
            throw new UndeclaredThrowableException(e);
        }
        dist.setTarball(url);

        return npmPackageDesc;
    }

    private void fetchShasum(Dist dist,
                             Map<String, RepositoryPath> checksumMap) {
        RepositoryPath shasumPath = checksumMap.get(MessageDigestAlgorithms.SHA_1);
        RepositoryPath integrityPath = checksumMap.get(MessageDigestAlgorithms.SHA_512);
        try {
            if (Objects.nonNull(shasumPath) && Files.exists(shasumPath)) {
                dist.setShasum(new String(Files.readAllBytes(shasumPath), StandardCharsets.UTF_8).trim());
            }
            if (Objects.nonNull(integrityPath) && Files.exists(integrityPath)) {
                dist.setIntegrity(new String(Files.readAllBytes(integrityPath), StandardCharsets.UTF_8).trim());
            }
        } catch (NoSuchFileException e) {
            logger.info("Checksum file not found [{}].", shasumPath);
        } catch (IOException e) {
            throw new UndeclaredThrowableException(e);
        }
    }

    protected String getRepositoryBaseUrl(Repository repository) {
        final String api = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? "ohpm" : "npm";
        return String.format("%s/artifactory/api/%s/%s/", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), api, repository.getId());
    }

}
