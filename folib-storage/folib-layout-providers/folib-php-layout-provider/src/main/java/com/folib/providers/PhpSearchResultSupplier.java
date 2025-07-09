package com.folib.providers;

import com.folib.artifact.coordinates.PhpCoordinates;
import com.folib.domain.Artifact;
import com.folib.php.PhpSearchPackage;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Path;
import java.util.function.Function;

/**
 * @author veadan
 */
@Component
public class PhpSearchResultSupplier implements Function<Path, PhpSearchPackage> {

    private static final Logger logger = LoggerFactory.getLogger(PhpSearchResultSupplier.class);

    public static final String SEARCH_DATE_FORMAT = "EEE MMM dd yyyy HH:mm:SS ZZZ (zzz)";

    @Override
    public PhpSearchPackage apply(Path path) {
        RepositoryPath repositoryPath = (RepositoryPath) path;

        PhpCoordinates c;
        Artifact artifactEntry;
        try {
            c = (PhpCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
            artifactEntry = repositoryPath.getArtifactEntry();
        } catch (IOException e) {
            throw new UndeclaredThrowableException(e);
        }
        PhpSearchPackage phpSearchPackage = PhpSearchPackage.builder().name(c.getName()).description(c.getDescription()).build();
        return phpSearchPackage;
    }

}
