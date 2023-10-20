package com.veadan.folib.ws.client.handler.command;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:35
 * @since x.x.x
 */
public interface FolibWsClientCommand<T>
{
    String command();
    
    void execute(T t);
}
