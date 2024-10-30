

package com.veadan.folib.scanner.common.msg;

import com.fasterxml.jackson.annotation.JsonView;
import com.veadan.folib.storage.Views;

import java.util.List;

/**
 * ${DESCRIPTION}
 *
 * @author Veadan
 * @version 2018-06-14 22:40
 */
public class TableResultResponse<T> extends BaseResponse {

    @JsonView(Views.ShortStorage.class)
    TableData<T> data;

    public TableResultResponse(long total, List<T> rows) {
        this.data = new TableData<T>(total, rows);
    }

    public TableResultResponse() {
        this.data = new TableData<T>();
    }

    TableResultResponse<T> total(int total){
        this.data.setTotal(total);
        return this;
    }

    TableResultResponse<T> total(List<T> rows) {
        this.data.setRows(rows);
        return this;
    }

    public TableData<T> getData() {
        return data;
    }

    public void setData(TableData<T> data) {
        this.data = data;
    }

    public class TableData<T> {
        @JsonView(Views.ShortStorage.class)
        long total;
        @JsonView(Views.ShortStorage.class)
        List<T> rows;

        public TableData(long total, List<T> rows) {
            this.total = total;
            this.rows = rows;
        }

        public TableData() {
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
