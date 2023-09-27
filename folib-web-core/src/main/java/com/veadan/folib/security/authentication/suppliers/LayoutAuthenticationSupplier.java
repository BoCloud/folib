package com.veadan.folib.security.authentication.suppliers;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.CacheUtil;
import org.springframework.core.annotation.Order;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.util.Objects;

import static com.veadan.folib.web.Constants.ARTIFACT_ROOT_PATH;

@Order(2)
public abstract class LayoutAuthenticationSupplier
        implements AuthenticationSupplier {

    @Inject
    private ConfigurationManager configurationManager;

    private String layoutAlias;

    public LayoutAuthenticationSupplier(String layoutAlias) {
        this.layoutAlias = layoutAlias;
    }

    @Override
    public boolean supports(@Nonnull HttpServletRequest request) {
        String uri = request.getRequestURI();
        if (!uri.startsWith(ARTIFACT_ROOT_PATH)) {
            return false;
        }

        String[] pathParts = uri.split("/");
        if (pathParts.length < 4) {
            return false;
        }

        String storageId = pathParts[2];
        String repositoryId = pathParts[3];
        if (storageId == null || repositoryId == null) {
            return false;
        }
        CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
        String key = String.format("%s:%s", storageId, repositoryId);
        Repository repository = cacheUtil.get(key);
        if (Objects.isNull(repository)) {
            Storage storage = configurationManager.getConfiguration().getStorage(storageId);
            if (storage == null) {
                return false;
            }
            repository = storage.getRepository(repositoryId);
            if (repository == null) {
                return false;
            }
            cacheUtil.put(key, repository);
        }
        return layoutAlias.equals(repository.getLayout());
    }
}
