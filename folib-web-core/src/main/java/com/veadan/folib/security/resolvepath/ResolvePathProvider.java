package com.veadan.folib.security.resolvepath;

import com.veadan.folib.storage.repository.Repository;

/**
 * @author veadan
 **/
public interface ResolvePathProvider {

    /**
     * 注册
     */
    void register();

    /**
     * 解析路径
     *
     * @param repository   仓库信息
     * @param relativePath 路径
     * @return 解析后的路径
     */
    String resolvePath(Repository repository, String relativePath);
}
