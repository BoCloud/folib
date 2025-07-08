package com.folib.service;

import com.folib.storage.repository.Repository;

/**
 * @author veadan
 * @date 2023/9/21 15:24
 */
public interface CocoapodsIndexService 
{
    boolean getSyncProxyIndexLock(Repository repository);
    
    boolean syncProxyIndex(Repository repository);
}
