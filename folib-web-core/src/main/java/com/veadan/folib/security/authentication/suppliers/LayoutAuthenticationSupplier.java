package com.veadan.folib.security.authentication.suppliers;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.CacheUtil;
import com.veadan.folib.web.Constants;
import org.apache.commons.lang3.StringUtils;
import org.springframework.core.annotation.Order;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.util.Objects;

import static com.veadan.folib.web.Constants.ARTIFACT_ROOT_PATH;
import static com.veadan.folib.web.Constants.ARTIFACT_ROOT_PATHS;

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
        if (ARTIFACT_ROOT_PATHS.stream().noneMatch(uri::startsWith)) {
            return false;
        }
        String repositoryId = null, storageId = null;
        String[] pathParts = uri.split("/");
        if (uri.startsWith(ARTIFACT_ROOT_PATH)) {
            if (pathParts.length < 4) {
                return false;
            }
            storageId = pathParts[2];
            repositoryId = pathParts[3];
            return checkLayoutAlias(storageId, repositoryId);
        }
        if (pathParts.length < 3) {
            return false;
        }
        for (String apiRootPath : Constants.ARTIFACT_API_ROOT_PATHS) {
            if (uri.startsWith(apiRootPath)) {
                String path = uri.replace(apiRootPath, "");
                String[] paths = path.split("/");
                //获取对应的存储空间
                repositoryId = paths[0];
                storageId = getDefaultStorageId(repositoryId);
                return checkLayoutAlias(storageId, repositoryId);
            }
        }
        //获取对应的存储空间
        repositoryId = pathParts[2];
        storageId = getDefaultStorageId(repositoryId);
        return checkLayoutAlias(storageId, repositoryId);
    }

    private boolean checkLayoutAlias(String storageId, String repositoryId) {
        if (StringUtils.isBlank(storageId) || StringUtils.isBlank(repositoryId)) {
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


    /**
     * 获取设置默认的存储空间
     *
     * @param repositoryId 仓库名称
     * @return 存储空间
     */
    public String getDefaultStorageId(String repositoryId) {
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        if (StringUtils.isNotBlank(repositoryId)) {
            //按照仓库查询对应的存储空间
            String key = "JFrogAdapterStorage_" + repositoryId;
            String jFrogAdapterStorage = distributedCacheComponent.get(key);
            if (StringUtils.isNotBlank(jFrogAdapterStorage)) {
                return jFrogAdapterStorage;
            }
        }
        String key = "JFrogAdapterDefaultStorage";
        String jFrogAdapterDefaultStorage = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(jFrogAdapterDefaultStorage)) {
            throw new RuntimeException("Default storage not found,Please Set the default storageId");
        }
        return jFrogAdapterDefaultStorage;
    }
}
