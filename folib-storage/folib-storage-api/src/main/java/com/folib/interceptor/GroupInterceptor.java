package com.folib.interceptor;

import com.folib.providers.io.RepositoryPath;

/**
 * @author veadan
 * @since 2025-03-07 17:04
 */
public interface GroupInterceptor {
     boolean shouldInterceptor(RepositoryPath path);
     void calculateIndex(RepositoryPath path);

}
