package com.veadan.folib.providers.layout;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.npm.metadata.Dependency;
import com.veadan.folib.npm.metadata.Dist;
import com.veadan.folib.npm.metadata.PackageVersion;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ArtifactTagService;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
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
    private ArtifactTagService artifactTagService;

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

        PackageVersion npmPackage = new PackageVersion();
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
            url = layoutProvider.resolveResource(repositoryPath).toString();
        } catch (IOException e) {
            throw new UndeclaredThrowableException(e);
        }
        dist.setTarball(url);

        if (artifactEntry.getTagSet().contains(artifactTagService.findOneOrCreate(ArtifactTag.LAST_VERSION))) {
            npmPackageDesc.setLastVersion(true);
        }
        return npmPackageDesc;
    }

    private void fetchShasum(Dist dist,
                             Map<String, RepositoryPath> checksumMap) {
        RepositoryPath shasumPath = checksumMap.get(MessageDigestAlgorithms.SHA_1);
        if (shasumPath == null || !Files.exists(shasumPath)) {
            return;
        }

        try {
            dist.setShasum(new String(Files.readAllBytes(shasumPath), "UTF-8").trim());
        } catch (NoSuchFileException e) {
            logger.debug("Checksum file not found [{}].", shasumPath);
        } catch (IOException e) {
            throw new UndeclaredThrowableException(e);
        }
    }

}
