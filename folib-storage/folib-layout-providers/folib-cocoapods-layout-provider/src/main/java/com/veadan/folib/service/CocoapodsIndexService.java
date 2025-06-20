package com.veadan.folib.service;

import com.veadan.folib.storage.repository.Repository;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/9/21 15:24
 * @since x.x.x
 */
public interface CocoapodsIndexService 
{
    boolean getSyncProxyIndexLock(Repository repository);
    
    boolean syncProxyIndex(Repository repository);
}
