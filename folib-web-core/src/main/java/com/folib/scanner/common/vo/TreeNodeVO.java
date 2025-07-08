

package com.folib.scanner.common.vo;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by Veadan on 2017/6/12.
 */
public class TreeNodeVO<T> {
    protected Object id;
    protected Object parentId;

    public List<T> getChildren() {
        return children;
    }

    public void setChildren(List<T> children) {
        this.children = children;
    }

    List<T> children = new ArrayList<T>();

    public Object getId() {
        return id;
    }

    public void setId(Object id) {
        this.id = id;
    }

    public Object getParentId() {
        return parentId;
    }

    public void setParentId(Object parentId) {
        this.parentId = parentId;
    }

    public void add(T node){
        children.add(node);
    }

}
