/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */


package com.folib.scanner.common.msg;

/**
 * Created by Veadan on 2018/6/11.
 */
public class ObjectRestResponse<T> extends BaseResponse {
    T data;
    boolean rel = true;

    public boolean isRel() {
        return rel;
    }

    public void setRel(boolean rel) {
        this.rel = rel;
    }


    public ObjectRestResponse rel(boolean rel) {
        this.setRel(rel);
        return this;
    }


    public ObjectRestResponse data(T data) {
        this.setData(data);
        return this;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }

    public ObjectRestResponse(boolean rel, String message) {
        this.rel=rel;
        this.setMessage(message);
    }

    public ObjectRestResponse(boolean rel,T data, String message) {
        this.rel=rel;
        this.data=data;
        this.setMessage(message);
    }

    public ObjectRestResponse(boolean rel, String message,int status) {
        this.rel=rel;
        super.setStatus(status);
        this.setMessage(message);
    }

    public  ObjectRestResponse(){};

    public static ObjectRestResponse ok(Object data) {
        return new ObjectRestResponse<Object>().data(data);
    }

    public static ObjectRestResponse ok() {
        return new ObjectRestResponse<Object>();
    }
}
