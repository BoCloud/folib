package com.veadan.folib.interceptor;

import com.veadan.folib.providers.io.RepositoryPath;

/**
 * @author huayanjun
 * @since 2025-03-07 17:04
 */
public interface GroupInterceptor {
     boolean shouldInterceptor(RepositoryPath path);
     void calculateIndex(RepositoryPath path);

}
