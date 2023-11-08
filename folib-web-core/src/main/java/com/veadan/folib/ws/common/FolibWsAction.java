package com.veadan.folib.ws.common;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:27
 * @since x.x.x
 */
@Data
@Accessors(chain = true)
public class FolibWsAction implements JsonEncoder
{
    private String command;
    private String payload;
}
