package com.veadan.folib.security.authentication.suppliers;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.springframework.core.annotation.Order;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static com.veadan.folib.web.Constants.ARTIFACT_ROOT_PATH;

@Order(2)
public abstract class LayoutAuthenticationSupplier
        implements AuthenticationSupplier
{

    @Inject
    private ConfigurationManager configurationManager;

    private String layoutAlias;

    private final Cache<String, String> layoutCache = CacheBuilder.newBuilder()
            .expireAfterWrite(5, TimeUnit.MINUTES)
            .build();

    public LayoutAuthenticationSupplier(String layoutAlias)
    {
        this.layoutAlias = layoutAlias;
    }

    @Override
    public boolean supports(@Nonnull HttpServletRequest request)
    {
        String uri = request.getRequestURI();
        if (!uri.startsWith(ARTIFACT_ROOT_PATH))
        {
            return false;
        }

        String[] pathParts = uri.split("/");
        if (pathParts.length < 4)
        {
            return false;
        }

        String storageId = pathParts[2];
        String repositoryId = pathParts[3];
        if (storageId == null || repositoryId == null)
        {
            return false;
        }
        String key = String.format("%s:%s", storageId, repositoryId);
        String layout = layoutCache.getIfPresent(key);
        if (StringUtils.isBlank(layout)) {
            Storage storage = configurationManager.getConfiguration().getStorage(storageId);
            if (storage == null) {
                return false;
            }
            Repository repository = storage.getRepository(repositoryId);
            if (repository == null) {
                return false;
            }
            layout = repository.getLayout();
            layoutCache.put(key, layout);
        }
        return layoutAlias.equals(layout);
    }
}
