

package com.veadan.folib.scanner.common.util;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * 查询参数
 */
@ApiModel("分页查询参数对象")
public class Query extends LinkedHashMap<String, Object> {
	private static final long serialVersionUID = 1L;
	//当前页码
    @ApiModelProperty("当前页")
    private int page = 1;
    //每页条数
    @ApiModelProperty("每页数量")
    private int limit = 10;

    public Query(Map<String, Object> params){
        this.putAll(params);
        //分页参数
        if(params.get("page")!=null) {
            this.page = Integer.parseInt(params.get("page").toString());
        }
        if(params.get("limit")!=null) {
            this.limit = Integer.parseInt(params.get("limit").toString());
        }
//        this.remove("page");
//        this.remove("limit");
    }


    public int getPage() {
        return page;
    }

    public void setPage(int page) {
        this.page = page;
    }

    public int getLimit() {
        return limit;
    }

    public void setLimit(int limit) {
        this.limit = limit;
    }
}
