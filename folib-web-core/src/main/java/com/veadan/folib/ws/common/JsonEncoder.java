package com.veadan.folib.ws.common;

import com.alibaba.fastjson.JSON;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/24 14:32
 * @since x.x.x
 */
public interface JsonEncoder
{
    default String encode()
    {
        return JSON.toJSONString(this);
    }
}
