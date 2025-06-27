package com.veadan.folib.ws.common;

/**
 * @author veadan
 * @date 2023/10/19 16:35
 */
public interface FolibWsCommand<T>
{
    String command();
    
    void execute(T t);
}
