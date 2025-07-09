package com.folib.artifact.coordinates;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.metadata.nuget.NugetFormatException;
import com.folib.storage.metadata.nuget.Nupkg;
import com.folib.storage.metadata.nuget.Nuspec;
import com.folib.storage.metadata.nuget.Nuspec.Metadata;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;

public class PathNupkg implements Nupkg
{

    private static final Logger logger = LoggerFactory.getLogger(PathNupkg.class);

    private RepositoryPath path;
    private Nuspec nuspecFile;
    private String hash;
    private NugetCoordinates artifactCoordinates;
    
    public PathNupkg(RepositoryPath path)
        throws NugetFormatException,
        UnsupportedEncodingException,
        IOException
    {
        Assert.notNull(path, "path should not be null");
        Assert.notNull(path.getArtifactEntry(), "artifact entry should not be null");
        
        this.path = path;
        this.artifactCoordinates = (NugetCoordinates) path.getArtifactEntry().getArtifactCoordinates();
        this.nuspecFile = createNuspecFile();
        this.hash = createHash();
    }

    public RepositoryPath getPath()
    {
        return path;
    }

    @Override
    public String getFileName()
    {
        return path.getFileName().toString();
    }

    @Override
    public String getHash()
    {
        return hash;
    }

    private String createHash()
        throws IOException,
        UnsupportedEncodingException
    {
        Map<String, RepositoryPath> checksumPathMap = path.getFileSystem().provider().resolveChecksumPathMap(path);
        if (checksumPathMap.isEmpty())
        {
            return null;
        }
        // Nuget package should have only one checksum digest algorithm.
        RepositoryPath checkSumPath = checksumPathMap.values().iterator().next();
        if (!Files.exists(checkSumPath))
        {
            logger.trace("Failed to resolve checksum file for [{}]", path);
            return "";
        }
        List<String> checkSumContents = Files.readAllLines(checkSumPath);
        if (checkSumContents.isEmpty() || checkSumContents.size() > 1)
        {
            logger.error("Found illegal checksum contents for [{}]", path);
            return null;
        }
        
        String checkSumStr = checkSumContents.iterator().next();
        
        return checkSumStr;
    }

    @Override
    public Nuspec getNuspec()
        throws NugetFormatException
    {
        return nuspecFile;
    }

    private Nuspec createNuspecFile()
        throws NugetFormatException
    {
        RepositoryPath nuspecPath = path.resolveSibling(artifactCoordinates.getId() + ".nuspec");
        if (!Files.exists(nuspecPath))
        {
            logger.trace("Failed to resolve .nuspec file for [{}]", path);
            Nuspec result = new Nuspec();
            Metadata metadata = result.getMetadata();
            metadata.id = artifactCoordinates.getId();
            metadata.version = SemanticVersion.parse(artifactCoordinates.getVersion());
            metadata.title = metadata.id;
            return result;
        }
        
        try (InputStream inputStream = Files.newInputStream(nuspecPath))
        {
            return Nuspec.parse(inputStream);
        }
        catch (IOException e)
        {
            logger.error("Failed to read .nuspec file for [{}]", path, e);
            return null;
        }
    }

    @Override
    public Long getSize()
    {
        try
        {
            return path.getArtifactEntry().getSizeInBytes();
        }
        catch (IOException e)
        {
            return 0L;
        }
    }

    @Override
    public InputStream getStream()
        throws IOException
    {
        return Files.newInputStream(path);
    }

    @Override
    public Date getUpdated()
    {
        try
        {
            return Date.from(path.getArtifactEntry().getLastUpdated().atZone(ZoneId.systemDefault()).toInstant());
        }
        catch (IOException e)
        {
            return null;
        }
    }

    @Override
    public String getId()
    {
        return artifactCoordinates.getId();
    }

    @Override
    public SemanticVersion getVersion()
    {
        try
        {
            return SemanticVersion.parse(artifactCoordinates.getVersion());
        }
        catch (Exception e)
        {
            logger.error("Failed to parse version for [{}]", path);
            return null;
        }
    }

    @Override
    public boolean equals(Object obj)
    {
        if (!(obj instanceof PathNupkg)) {
            return false;
        }
        return path.equals(((PathNupkg)obj).path);
    }

    @Override
    public int hashCode()
    {
        return path.hashCode();
    }

}
