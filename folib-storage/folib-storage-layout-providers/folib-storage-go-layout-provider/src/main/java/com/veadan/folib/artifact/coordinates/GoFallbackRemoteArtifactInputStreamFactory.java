package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.repository.proxied.FallbackRemoteArtifactInputStreamFactory;
import org.springframework.stereotype.Component;

import java.io.InputStream;
import java.util.function.Function;

/**
 * @author pengYongQiang
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

    public static void main(String[] args) {
        String s = "github.com/pengyongqiang666/hello-world-go-private/@v/list";
        if (s.endsWith("/@v/list")){
            String substring = s.substring(0, s.length()-"/@v/list".length());
            System.out.println(substring);
        }

    }
    @Override
    protected String getLayout() {
        return "go";
    }
}
