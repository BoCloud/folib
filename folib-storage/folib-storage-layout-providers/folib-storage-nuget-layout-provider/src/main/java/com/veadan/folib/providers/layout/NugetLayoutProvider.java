package com.veadan.folib.providers.layout;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.veadan.folib.artifact.coordinates.NugetArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.NugetRepositoryFeatures;
import com.veadan.folib.repository.NugetRepositoryManagementStrategy;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Layout provider for Nuget package repository.<br>
 * It provides hierarchical directory layout like follows: <br>
 * &lt;packageID&gt;<br>
 * └─&lt;version&gt;<br>
 * &emsp;├─&lt;packageID&gt;.&lt;version&gt;.nupkg<br>
 * &emsp;├─&lt;packageID&gt;.&lt;version&gt;.nupkg.sha512<br>
 * &emsp;└─&lt;packageID&gt;.nuspec<br>
 * 
 * 
 * @author @author veadan
 *
 */
@Component
public class NugetLayoutProvider
        extends AbstractLayoutProvider<NugetArtifactCoordinates>
{
    private static final Logger logger = LoggerFactory.getLogger(NugetLayoutProvider.class);

    public static final String ALIAS = NugetArtifactCoordinates.LAYOUT_NAME;

    public static final String USER_AGENT_PREFIX = ALIAS;

    @Inject
    private NugetRepositoryManagementStrategy nugetRepositoryManagementStrategy;

    @Inject
    private NugetRepositoryFeatures nugetRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public NugetArtifactCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return NugetArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath path)
    {
        return path.getFileName().toString().endsWith(".nuspec");
    }

    protected String toBase64(byte[] digest)
    {
        byte[] encoded = Base64.getEncoder()
                               .encode(digest);
        return new String(encoded, StandardCharsets.UTF_8);
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return nugetRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @Override
    public Set<String> getDigestAlgorithmSet()
    {
        return Stream.of(MessageDigestAlgorithms.SHA_512)
                     .collect(Collectors.toSet());
    }

    @Override
    public NugetRepositoryManagementStrategy getRepositoryManagementStrategy()
    {
        return nugetRepositoryManagementStrategy;
    }

}
