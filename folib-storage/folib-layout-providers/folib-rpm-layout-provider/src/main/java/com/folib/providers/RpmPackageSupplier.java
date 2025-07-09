package com.folib.providers;

import com.folib.artifact.coordinates.RpmCoordinates;
import com.folib.domain.Artifact;
import com.folib.npm.metadata.PackageEntry;
import com.folib.npm.metadata.SearchResult;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
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

        RpmCoordinates c;
        Artifact artifactEntry;
        try
        {
            c = (RpmCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
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
