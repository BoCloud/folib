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
package com.folib.domain;

import java.util.List;

/**
 * @author veadan
 * @date 2023/3/17
 **/
public class PageResultResponse<T> {

    Data<T> data;

    public PageResultResponse(long total, List<T> rows) {
        this.data = new Data<T>(total, rows);
    }

    public PageResultResponse() {
        this.data = new Data<T>();
    }

    PageResultResponse<T> total(int total) {
        this.data.setTotal(total);
        return this;
    }

    PageResultResponse<T> total(List<T> rows) {
        this.data.setRows(rows);
        return this;
    }

    public Data<T> getData() {
        return data;
    }

    public void setData(Data<T> data) {
        this.data = data;
    }



    public class Data<T> {
        long total;
        List<T> rows;

        public Data(long total, List<T> rows) {
            this.total = total;
            this.rows = rows;
        }

        public Data() {
        }

        public long getTotal() {
            return total;
        }

        public void setTotal(long total) {
            this.total = total;
        }

        public List<T> getRows() {
            return rows;
        }

        public void setRows(List<T> rows) {
            this.rows = rows;
        }
    }
}


