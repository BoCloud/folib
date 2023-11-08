package com.veadan.folib.ws.common;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:35
 * @since x.x.x
 */
public interface FolibWsCommand<T>
{
    String command();
    
    void execute(T t);
}
