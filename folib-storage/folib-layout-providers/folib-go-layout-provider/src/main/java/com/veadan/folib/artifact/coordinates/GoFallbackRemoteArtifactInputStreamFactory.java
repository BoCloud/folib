package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.repository.proxied.FallbackRemoteArtifactInputStreamFactory;
import org.springframework.stereotype.Component;

import java.io.InputStream;
import java.util.function.Function;

/**
 * @author veadan
 * @date 1/15/2024 14:30
 */
@Component()
public class GoFallbackRemoteArtifactInputStreamFactory extends FallbackRemoteArtifactInputStreamFactory {

    @Override
    protected Function<Exception, InputStream> getFallbackRemoteArtifactInputStream(RepositoryPath repositoryPath) {

        return e -> {
            if (!(e instanceof ArtifactNotFoundException)){
                return null;
            }
            return new GoFallbackRemoteArtifactInputStream(repositoryPath);
        };
    }

    @Override
    protected String getLayout() {
        return "go";
    }
}
