package com.veadan.folib.ws.common;

import com.alibaba.fastjson.JSON;

import java.io.Serializable;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/24 14:32
 * @since x.x.x
 */
public interface JsonEncoder extends Serializable
{
    default String encode()
    {
        return JSON.toJSONString(this);
    }
}
