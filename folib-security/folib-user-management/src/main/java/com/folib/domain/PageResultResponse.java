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


