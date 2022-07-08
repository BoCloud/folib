package com.veadan.folib.web;

import com.veadan.folib.configuration.StoragesConfigurationManager;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;

import javax.servlet.http.HttpServletRequest;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;

import org.springframework.web.servlet.mvc.condition.AbstractRequestCondition;
import static com.veadan.folib.web.Constants.ARTIFACT_ROOT_PATH;

/**
 * @author xuxinping
 * @author veadan
 */
public class LayoutRequestCondition
        extends AbstractRequestCondition<ExposableRequestCondition>
{

    private static final String ARTIFACT_COPY_PATH = ARTIFACT_ROOT_PATH + "/copy";

    protected final String layout;
    protected final StoragesConfigurationManager configurationManager;

    public LayoutRequestCondition(StoragesConfigurationManager configurationManager,
                                  String layout)
    {
        this.layout = layout;
        this.configurationManager = configurationManager;
    }

    @Override
    public ExposableRequestCondition combine(ExposableRequestCondition other)
    {
        return other;
    }

    @Override
    public ExposableRequestCondition getMatchingCondition(HttpServletRequest request)
    {
        String servletPath = Optional.ofNullable(request.getServletPath()).filter(
                s -> s != null && s.trim().length() > 0).orElse(request.getPathInfo());
        if (servletPath.startsWith(ARTIFACT_COPY_PATH))
        {
            return getPathCopyCondition(request);
        }
        else if (servletPath.startsWith(ARTIFACT_ROOT_PATH))
        {
            return getStorageAndRepositoryCondition(servletPath);
        }

        return null;
    }

    private ExposableRequestCondition getPathCopyCondition(HttpServletRequest request)
    {
        String storageId = request.getParameter("srcStorageId");
        if (storageId == null)
        {
            return null;
        }
        String repositoryId = request.getParameter("srcRepositoryId");
        if (repositoryId == null)
        {
            return null;
        }

        return getStorageAndRepositoryCondition(storageId, repositoryId);
    }

    private ExposableRequestCondition getStorageAndRepositoryCondition(String servletPath)
    {
        String[] pathParts = servletPath.split("/");
        if (pathParts.length < 4)
        {
            return null;
        }

        String storageId = pathParts[2];
        String repositoryId = pathParts[3];

        return getStorageAndRepositoryCondition(storageId, repositoryId);
    }

    private ExposableRequestCondition getStorageAndRepositoryCondition(String storageId,
                                                                       String repositoryId)
    {
        Storage storage = configurationManager.getStorage(storageId);
        if (storage == null)
        {
            return new StorageNotFoundRequestCondition(storageId);
        }
        Repository repository = configurationManager.getRepository(storageId, repositoryId);
        if (repository == null)
        {
            return new RepositoryNotFoundRequestCondition(repositoryId);
        }

        String requestedLayout = repository.getLayout();
        if (!layout.equals(requestedLayout))
        {
            return null;
        }

        return new RepositoryRequestCondition(repository);
    }

    @Override
    public int compareTo(ExposableRequestCondition other,
                         HttpServletRequest request)
    {
        return 1;
    }

    @Override
    protected Collection<?> getContent()
    {
        return Collections.singleton(layout);
    }

    @Override
    protected String getToStringInfix()
    {
        return layout;
    }

}
