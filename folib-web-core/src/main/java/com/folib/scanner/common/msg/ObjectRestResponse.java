

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
