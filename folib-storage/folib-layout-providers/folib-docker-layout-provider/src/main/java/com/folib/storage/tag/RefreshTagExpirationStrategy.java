package com.folib.storage.tag;

import com.folib.providers.io.RepositoryPath;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.IOException;

/**
 * @author veadan
 * @date 2024/1/19
 **/
@Slf4j
@Component
public class RefreshTagExpirationStrategy
        implements DockerExpirationStrategy {

    @Override
    public Decision decide(RepositoryPath repositoryPath)
            throws IOException {
        return Decision.EXPIRED;
    }
}
