package com.veadan.folib.controllers.layout.maven;

import com.veadan.folib.storage.indexing.IndexTypeEnum;
import com.veadan.folib.storage.repository.Repository;

import java.util.function.Function;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.apache.maven.index.context.IndexingContext;

/**
 * We need to support .index/nexus-maven-repository-index.XXX.gz requests
 * but the real path is under `local` or `remote` sub-path
 *
 * @author veadan
 */
public class MavenRepositoryIndexPathTransformer
        implements Function<String, String>
{

    private static final String INDEX_BASE_PATH = ".index/";

    private static final String INDEX_LOCAL_BASE_PATH = INDEX_BASE_PATH + IndexTypeEnum.LOCAL.getType() + "/";

    private static final String INDEX_REMOTE_BASE_PATH = INDEX_BASE_PATH + IndexTypeEnum.REMOTE.getType() + "/";

    private static final Pattern INDEX_FILE_PATTERN = Pattern.compile(
            "^" + INDEX_BASE_PATH + "(" + IndexingContext.INDEX_FILE_PREFIX + "(\\.[0-9]+)?" + ".gz|" +
            IndexingContext.INDEX_REMOTE_PROPERTIES_FILE + ")$");

    private final Repository repository;

    MavenRepositoryIndexPathTransformer(final Repository repository)
    {
        this.repository = repository;
    }

    @Override
    public String apply(final String requestedPath)
    {
        String path = requestedPath;

        if (INDEX_FILE_PATTERN.matcher(path).matches())
        {
            path = repository.isProxyRepository() ? INDEX_REMOTE_BASE_PATH : INDEX_LOCAL_BASE_PATH;
            path += StringUtils.substringAfterLast(requestedPath, INDEX_BASE_PATH);
        }
        return path;
    }
}
