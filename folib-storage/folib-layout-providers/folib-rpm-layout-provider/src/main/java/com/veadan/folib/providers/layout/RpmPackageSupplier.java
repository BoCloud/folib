package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.RpmArtifactCoordinates;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.npm.metadata.PackageEntry;
import com.veadan.folib.npm.metadata.SearchResult;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Path;
import java.time.ZoneId;
import java.util.Date;
import java.util.function.Function;

@Component
public class RpmPackageSupplier implements Function<Path, SearchResult>
{

    private static final Logger logger = LoggerFactory.getLogger(RpmPackageSupplier.class);

    public static final String SEARCH_DATE_FORMAT = "EEE MMM dd yyyy HH:mm:SS ZZZ (zzz)";

    @Override
    public SearchResult apply(Path path)
    {
        RepositoryPath repositoryPath = (RepositoryPath) path;

        RpmArtifactCoordinates c;
        Artifact artifactEntry;
        try
        {
            c = (RpmArtifactCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
            artifactEntry = repositoryPath.getArtifactEntry();
        }
        catch (IOException e)
        {
            throw new UndeclaredThrowableException(e);
        }

        SearchResult searchResult = new SearchResult();

        PackageEntry packageEntry = new PackageEntry();
        searchResult.setPackage(packageEntry);

        packageEntry.setDate(Date.from(artifactEntry.getLastUpdated().atZone(ZoneId.systemDefault()).toInstant()));

        packageEntry.setName(c.getName());
        packageEntry.setScope(c.getScope() == null ? "unscoped" : c.getScope());
        packageEntry.setVersion(c.getVersion());

        return searchResult;
    }

}
