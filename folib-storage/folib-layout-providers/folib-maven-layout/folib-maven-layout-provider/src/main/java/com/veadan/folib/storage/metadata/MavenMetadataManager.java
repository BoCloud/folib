package com.veadan.folib.storage.metadata;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.artifact.MavenArtifact;
import com.veadan.folib.artifact.MavenArtifactUtils;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathLock;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import org.apache.commons.collections4.CollectionUtils;
import com.veadan.folib.commons.io.MultipleDigestOutputStream;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.storage.metadata.maven.comparators.SnapshotVersionComparator;
import com.veadan.folib.storage.metadata.maven.comparators.VersionComparator;
import com.veadan.folib.storage.metadata.maven.versions.MetadataVersion;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryPolicyEnum;
import com.veadan.folib.storage.repository.UnknownRepositoryTypeException;

import jakarta.inject.Inject;
import java.io.*;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.locks.Lock;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.repository.metadata.Metadata;
import org.apache.maven.artifact.repository.metadata.Plugin;
import org.apache.maven.artifact.repository.metadata.SnapshotVersion;
import org.apache.maven.artifact.repository.metadata.Versioning;
import org.apache.maven.artifact.repository.metadata.io.xpp3.MetadataXpp3Reader;
import org.apache.maven.artifact.repository.metadata.io.xpp3.MetadataXpp3Writer;
import org.codehaus.plexus.util.WriterFactory;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Martin Todorov
 * @author Steve Todorov
 * @author @author veadan
 * @author Ekaterina Novik
 */
@Component
public class MavenMetadataManager
{

    private static final Logger logger = LoggerFactory.getLogger(MavenMetadataManager.class);

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    private RepositoryPathLock repositoryPathLock;


    public Metadata readMetadata(MavenArtifact artifact)
            throws IOException,
                   XmlPullParserException,
                   ProviderImplementationException
    {
        RepositoryPath repositoryPath = artifact.getPath();
        Repository repository = repositoryPath.getRepository();

        LayoutProvider layoutProvider = LayoutProviderRegistry.getLayoutProvider(repository, layoutProviderRegistry);
        if (!RepositoryFiles.artifactExists(repositoryPath))
        {
            throw new IOException("Artifact " + artifact.toString() + " does not exist in repository " + repository +
                                  " !");

        }

        Path artifactBasePath = repositoryPath;
        if (artifact.getVersion() != null)
        {
            artifactBasePath = repositoryPath.getParent().getParent();
        }

        logger.debug("Getting metadata for {}", artifactBasePath);

        return readMetadata(artifactBasePath);
    }

    public Metadata readMetadata(Path artifactBasePath)
            throws IOException, XmlPullParserException
    {
        Path metadataPath = MetadataHelper.getMetadataPath(artifactBasePath);
        Metadata metadata;

        try (InputStream is = Files.newInputStream(metadataPath))
        {
            metadata = readMetadata(is);
        }

        return metadata;
    }

    public Metadata readMetadata(InputStream is)
            throws IOException, XmlPullParserException
    {
        Metadata metadata;

        try (InputStream inputStream = is)
        {
            MetadataXpp3Reader reader = new MetadataXpp3Reader();

            metadata = reader.read(inputStream);
        }

        return metadata;
    }

    public void storeMetadata(final RepositoryPath metadataBasePath,
                              final String version,
                              final Metadata metadata,
                              final MetadataType metadataType) throws IOException
    {

        doInLock(metadataBasePath, path ->
                 {
                     try
                     {
                         Path metadataPath = MetadataHelper.getMetadataPath(metadataBasePath, version, metadataType);
                         if (Objects.nonNull(metadata)) {
                             if (CollectionUtils.isEmpty(metadata.getVersioning().getVersions()) && CollectionUtils.isEmpty(metadata.getVersioning().getSnapshotVersions()) && CollectionUtils.isEmpty(metadata.getPlugins())) {
                                 Files.deleteIfExists(metadataPath);
                                 return;
                             }
                         }
                         if (metadataPath.toString().startsWith("s3://")) {
                             try (
                                     OutputStream os = Files.newOutputStream(metadataPath,
                                             StandardOpenOption.CREATE,
                                             StandardOpenOption.TRUNCATE_EXISTING)) {
                                 Writer writer = WriterFactory.newXmlWriter(os);

                                 MetadataXpp3Writer mappingWriter = new MetadataXpp3Writer();
                                 mappingWriter.write(writer, metadata);

                                 os.flush();

                             } catch (Exception e) {
                                 e.printStackTrace();
                             }
                         } else {
                             try (OutputStream os = new MultipleDigestOutputStream(metadataPath,
                                     Files.newOutputStream(metadataPath,
                                             StandardOpenOption.CREATE,
                                             StandardOpenOption.TRUNCATE_EXISTING)))
                             {
                                 Writer writer = WriterFactory.newXmlWriter(os);

                                 MetadataXpp3Writer mappingWriter = new MetadataXpp3Writer();
                                 mappingWriter.write(writer, metadata);

                                 os.flush();

                             }
                         }
                     }
                     catch (Exception ex)
                     {
                         throw new UndeclaredThrowableException(ex);
                     }
                 }
        );
    }

    /**
     * Generate a metadata file for an artifact.
     */
    public void generateMetadata(RepositoryPath artifactGroupDirectoryPath,
                                 VersionCollectionRequest request)
            throws IOException,
                   ProviderImplementationException,
                   UnknownRepositoryTypeException
    {
        logger.debug("VersionCollectionRequest：{}", JSONObject.toJSONString(request));
        Repository repository = artifactGroupDirectoryPath.getRepository();
        LayoutProvider layoutProvider = LayoutProviderRegistry.getLayoutProvider(repository, layoutProviderRegistry);
        if (!RepositoryFiles.artifactExists(artifactGroupDirectoryPath))
        {
            logger.error("Artifact metadata generation failed: {}).", artifactGroupDirectoryPath);

            return;
        }

        logger.debug("Artifact metadata generation triggered for {} in '{}:{}' [policy: {}].",
                artifactGroupDirectoryPath, repository.getStorage().getId(), repository.getId(), repository.getPolicy());

        Pair<String, String> artifactGroup = MavenArtifactUtils.getDirectoryGA(artifactGroupDirectoryPath);
        String artifactGroupId = artifactGroup.getValue0();
        String artifactId = artifactGroup.getValue1();

        Metadata metadata = new Metadata();
        metadata.setGroupId(artifactGroupId);
        metadata.setArtifactId(artifactId);

        List<MetadataVersion> baseVersioning = request.getMetadataVersions();
        Versioning versioning = request.getVersioning();

        boolean hasSnapshot = false, hasRelease = false;
        if (!versioning.getVersions().isEmpty()) {
            hasSnapshot = versioning.getVersions().stream().anyMatch(ArtifactUtils::isSnapshot);
            hasRelease = versioning.getVersions().stream().anyMatch(version -> !ArtifactUtils.isSnapshot(version));
        }

        // Set lastUpdated tag for main maven-metadata
        MetadataHelper.setLastUpdated(versioning);

        /**
         * In a release repository we only need to generate maven-metadata.xml in the artifactBasePath
         * (i.e. org/foo/bar/maven-metadata.xml)
         */
        if (repository.getPolicy().equals(RepositoryPolicyEnum.RELEASE.getPolicy()))
        {
            // Don't write empty <versioning/> tags when no versions are available.
            if (!versioning.getVersions().isEmpty())
            {
                String latestVersion = baseVersioning.get(baseVersioning.size() - 1).getVersion();
                List<MetadataVersion> releaseVersionList = baseVersioning.stream().filter(item -> !ArtifactUtils.isSnapshot(item.getVersion())).collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(releaseVersionList)) {
                    versioning.setRelease(releaseVersionList.get(releaseVersionList.size() - 1).getVersion());
                }
                metadata.setVersioning(request.getVersioning());
                // Set <latest> by figuring out the most recent upload
                Collections.sort(baseVersioning);
                versioning.setLatest(latestVersion);
                metadata.setVersion(latestVersion);
            }

            // Touch the lastUpdated field
            MetadataHelper.setLastUpdated(versioning);

            // Write basic metadata
            storeMetadata(artifactGroupDirectoryPath, null, metadata, MetadataType.ARTIFACT_ROOT_LEVEL);

            logger.debug("Generated Maven metadata for {}:{}.", artifactGroupId, artifactId);
        }
        /**
         * In a snapshot repository we need to generate maven-metadata.xml in the artifactBasePath and
         * generate additional maven-metadata.xml files for each snapshot directory containing information about
         * all available artifacts.
         */
        else if (repository.getPolicy().equals(RepositoryPolicyEnum.SNAPSHOT.getPolicy()))
        {
            // Don't write empty <versioning/> tags when no versions are available.
            if (!versioning.getVersions().isEmpty())
            {
                // Set <latest>
                String latestVersion = versioning.getVersions().get(versioning.getVersions().size() - 1);
                versioning.setLatest(latestVersion);
                metadata.setVersion(latestVersion);
                metadata.setVersioning(versioning);

                // Generate and write additional snapshot metadata.
                for (String version : metadata.getVersioning().getVersions())
                {
                    RepositoryPath snapshotBasePath = artifactGroupDirectoryPath.toAbsolutePath()
                            .resolve(ArtifactUtils.toSnapshotVersion(version));

                    generateLastSnapshotVersioningMetadata(artifactGroupId, artifactId, snapshotBasePath,
                            version, true);
                }
            }

            // Write artifact metadata
            storeMetadata(artifactGroupDirectoryPath, null, metadata, MetadataType.ARTIFACT_ROOT_LEVEL);

            logger.debug("Generated Maven metadata for {}:{}.", artifactGroupId, artifactId);
        }
        else if (repository.getPolicy().equals(RepositoryPolicyEnum.MIXED.getPolicy()))
        {
            //Implement merging.
            Metadata releaseMetadata = null;
            if (hasRelease) {
                releaseMetadata = new Metadata();
                releaseMetadata.setGroupId(artifactGroupId);
                releaseMetadata.setArtifactId(artifactId);
                // Don't write empty <versioning/> tags when no versions are available.
                if (!versioning.getVersions().isEmpty())
                {
                    String latestVersion = baseVersioning.get(baseVersioning.size() - 1).getVersion();

                    List<MetadataVersion> releaseVersionList = baseVersioning.stream().filter(item -> !ArtifactUtils.isSnapshot(item.getVersion())).collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(releaseVersionList)) {
                        versioning.setRelease(releaseVersionList.get(releaseVersionList.size() - 1).getVersion());
                    }
                    releaseMetadata.setVersioning(request.getVersioning());

                    // Set <latest> by figuring out the most recent upload
                    Collections.sort(baseVersioning);
                    versioning.setLatest(latestVersion);
                    releaseMetadata.setVersion(latestVersion);

                }

                // Touch the lastUpdated field
                MetadataHelper.setLastUpdated(versioning);
                logger.debug("Generated Maven metadata for {}:{}.", artifactGroupId, artifactId);
            }

            Metadata snapshotMetadata = null;
            if (hasSnapshot) {
                snapshotMetadata = new Metadata();
                snapshotMetadata.setGroupId(artifactGroupId);
                snapshotMetadata.setArtifactId(artifactId);
                // Don't write empty <versioning/> tags when no versions are available.
                if (!versioning.getVersions().isEmpty())
                {
                    // Set <latest>
                    String latestVersion = versioning.getVersions().get(versioning.getVersions().size() - 1);
                    versioning.setLatest(latestVersion);
                    snapshotMetadata.setVersion(latestVersion);
                    snapshotMetadata.setVersioning(versioning);

                    // Generate and write additional snapshot metadata.
                    for (String version : snapshotMetadata.getVersioning().getVersions())
                    {
                        if (ArtifactUtils.isSnapshot(version)) {
                            RepositoryPath snapshotBasePath = artifactGroupDirectoryPath.toAbsolutePath()
                                    .resolve(ArtifactUtils.toSnapshotVersion(version));

                            generateLastSnapshotVersioningMetadata(artifactGroupId, artifactId, snapshotBasePath,
                                    version, true);
                        }
                    }
                }
                logger.debug("Generated Maven metadata for {}:{}.", artifactGroupId, artifactId);
            }
            if (Objects.nonNull(releaseMetadata) && Objects.nonNull(snapshotMetadata)) {
                mergeAndStore(artifactGroupDirectoryPath, releaseMetadata, snapshotMetadata);
            } else if (Objects.nonNull(releaseMetadata)) {
                storeMetadata(artifactGroupDirectoryPath, null, releaseMetadata, MetadataType.ARTIFACT_ROOT_LEVEL);
            } else if (Objects.nonNull(snapshotMetadata)) {
                storeMetadata(artifactGroupDirectoryPath, null, snapshotMetadata, MetadataType.ARTIFACT_ROOT_LEVEL);
            }
        }
        else
        {
            throw new UnknownRepositoryTypeException("Repository policy type unknown: " + repository.getId());
        }

        // If this is a plugin, we need to add an additional metadata to the groupId.artifactId path.
        if (!request.getPlugins().isEmpty())
        {
            generateMavenPluginMetadata(artifactGroupId, artifactId, artifactGroupDirectoryPath.getParent(),
                    request.getPlugins());
        }
    }

    private void generateMavenPluginMetadata(String groupId, String aritfactId, RepositoryPath pluginMetadataPath, List<Plugin> plugins) throws IOException
    {
        Metadata pluginMetadata = new Metadata();
        pluginMetadata.setPlugins(plugins);

        storeMetadata(pluginMetadataPath, null, pluginMetadata, MetadataType.PLUGIN_GROUP_LEVEL);

        logger.debug("Generated Maven plugin metadata for {}:{}.", groupId, aritfactId);
    }

    public Metadata generateSnapshotVersioningMetadata(String groupId,
                                                       String artifactId,
                                                       RepositoryPath snapshotBasePath,
                                                       String version,
                                                       boolean store)
            throws IOException
    {
        VersionCollector versionCollector = new VersionCollector();
        List<SnapshotVersion> snapshotVersions = versionCollector.collectTimestampedSnapshotVersions(snapshotBasePath);
        Versioning snapshotVersioning = versionCollector.generateSnapshotVersions(snapshotVersions);

        MetadataHelper.setupSnapshotVersioning(snapshotVersioning);

        // Last updated should be present in both cases.
        MetadataHelper.setLastUpdated(snapshotVersioning);

        // Write snapshot metadata version information for each snapshot.
        Metadata snapshotMetadata = new Metadata();
        snapshotMetadata.setGroupId(groupId);
        snapshotMetadata.setArtifactId(artifactId);
        snapshotMetadata.setVersioning(snapshotVersioning);

        // Set the version that this metadata represents, if any. This is used for artifact snapshots only.
        // http://maven.apache.org/ref/3.3.3/maven-repository-metadata/repository-metadata.html
        snapshotMetadata.setVersion(version);

        if (store)
        {
            storeMetadata(snapshotBasePath.getParent(), version, snapshotMetadata, MetadataType.SNAPSHOT_VERSION_LEVEL);
        }

        return snapshotMetadata;
    }

    public Metadata generateLastSnapshotVersioningMetadata(String groupId,
                                                       String artifactId,
                                                       RepositoryPath snapshotBasePath,
                                                       String version,
                                                       boolean store)
            throws IOException
    {
        VersionCollector versionCollector = new VersionCollector();
        List<SnapshotVersion> snapshotVersions = versionCollector.collectTimestampedSnapshotVersions(snapshotBasePath);
        //获取最新的快照版本，maven-metadata.xml中只保存最新的快照版本号
        String versionPrefix = version.replace(RepositoryPolicyEnum.SNAPSHOT.name(), "");
        snapshotVersions = snapshotVersions.stream().filter(snapshotVersion -> snapshotVersion.getVersion().startsWith(versionPrefix)).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(snapshotVersions)) {
            String lastVersion = snapshotVersions.get(snapshotVersions.size() - 1).getVersion();
            snapshotVersions = snapshotVersions.stream().filter(snapshotVersion ->  snapshotVersion.getVersion().equals(lastVersion)).collect(Collectors.toList());
        }

        Versioning snapshotVersioning = versionCollector.generateSnapshotVersions(snapshotVersions);

        MetadataHelper.setupSnapshotVersioning(snapshotVersioning);

        // Last updated should be present in both cases.
        MetadataHelper.setLastUpdated(snapshotVersioning);

        // Write snapshot metadata version information for each snapshot.
        Metadata snapshotMetadata = new Metadata();
        snapshotMetadata.setGroupId(groupId);
        snapshotMetadata.setArtifactId(artifactId);
        snapshotMetadata.setVersioning(snapshotVersioning);

        // Set the version that this metadata represents, if any. This is used for artifact snapshots only.
        // http://maven.apache.org/ref/3.3.3/maven-repository-metadata/repository-metadata.html
        snapshotMetadata.setVersion(version);

        if (store)
        {
            storeMetadata(snapshotBasePath.getParent(), version, snapshotMetadata, MetadataType.SNAPSHOT_VERSION_LEVEL);
        }

        return snapshotMetadata;
    }

    public void mergeAndStore(final RepositoryPath metadataBasePath,
                              final Metadata mergeMetadata) throws IOException
    {
        doInLock(metadataBasePath, path ->
        {
            if (Files.exists(metadataBasePath))
            {
                try
                {
                    final Metadata metadata = readMetadata(metadataBasePath);
                    mergeAndStore(metadataBasePath, metadata, mergeMetadata);
                    return;
                }
                catch (Exception e)
                {
                    // Exception not propagated, intentionally
                    logger.info("Unable to merge the metadata to {} by source metadata {}. " +
                                 "Exception message was: {}. Continuing with storing new metadata ...",
                                 metadataBasePath,
                                 ReflectionToStringBuilder.toString(mergeMetadata),
                                 e.getMessage(),
                                 e);
                }
            }

            try
            {
                Files.createDirectories(metadataBasePath);
                storeMetadata(metadataBasePath, null, mergeMetadata, MetadataType.ARTIFACT_ROOT_LEVEL);
            }
            catch (IOException e)
            {
                throw new UndeclaredThrowableException(e);
            }

        });
    }

    public void mergeAndStore(MavenArtifact artifact,
                              Metadata mergeMetadata)
            throws IOException,
                   XmlPullParserException,
                   ProviderImplementationException
    {
        RepositoryPath repositoryPath = artifact.getPath();
        Repository repository = repositoryPath.getRepository();

        LayoutProvider layoutProvider = LayoutProviderRegistry.getLayoutProvider(repository, layoutProviderRegistry);
        if (!RepositoryFiles.artifactExists(repositoryPath))
        {
            throw new IOException("Artifact " + artifact.toString() + " does not exist in repository " + repository +
                                  " !");
        }

        RepositoryPath artifactBasePath = repositoryPath.getParent().getParent();
        logger.debug("Artifact merge metadata triggered for {}({}). {}",
                     artifact, artifactBasePath, repository.getType());

        try
        {
            Metadata metadata = readMetadata(artifact);
            mergeAndStore(artifactBasePath, metadata, mergeMetadata);
        }
        catch (FileNotFoundException e)
        {
            logger.error(e.getMessage(), e);
            throw new IOException("Artifact " + artifact.toString() + " doesn't contain any metadata," +
                                  " therefore we can't merge the metadata!");
        }
    }

    public void mergeAndStore(final RepositoryPath metadataBasePath,
                              final Metadata metadata,
                              final Metadata mergeMetadata) throws IOException
    {
        doInLock(metadataBasePath, path ->
        {
            metadata.merge(mergeMetadata);

            Versioning versioning = metadata.getVersioning();
            if (versioning.getVersions() != null)
            {
                versioning.getVersions().sort(new VersionComparator());
            }
            if (versioning.getSnapshotVersions() != null)
            {
                versioning.getSnapshotVersions().sort(new SnapshotVersionComparator());
            }

            try
            {
                storeMetadata(metadataBasePath, null, metadata, MetadataType.ARTIFACT_ROOT_LEVEL);
            }
            catch (IOException e)
            {
                throw new UndeclaredThrowableException(e);
            }
        });
    }

    private void doInLock(RepositoryPath metadataBasePath,
                          Consumer<Path> operation) throws IOException
    {
        if (repositoryPathLock.lock(metadataBasePath)) {
            try
            {
                operation.accept(metadataBasePath);
            }
            finally
            {
                repositoryPathLock.unLock(metadataBasePath);
            }
        }
    }
}
