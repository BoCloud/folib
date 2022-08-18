package com.veadan.folib.scanner.rest;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.scanner.common.base.BaseController;
import com.veadan.folib.scanner.common.msg.ObjectRestResponse;
import com.veadan.folib.scanner.biz.FolibScannerBiz;
import com.veadan.folib.scanner.entity.FolibScanner;
import com.veadan.folib.scanner.service.ScanService;
import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/folibScanner")
@Api(tags ="")
public class FolibScannerController extends BaseController<FolibScannerBiz,FolibScanner,String> {

    @Autowired
    private ScanService scanService;

    @GetMapping("/update")
    public ObjectRestResponse updateDb(){

        scanService.updateDB();
        return new ObjectRestResponse(true,"更新中");
    }

    @GetMapping("/getCount")
    public ObjectRestResponse getCount(){
        JSONObject object = new JSONObject();
        object.put("denpendencyCount",this.baseBiz.getScanSum());
        object.put("totalCount",this.baseBiz.getTotalSum());
        return new ObjectRestResponse(true,object,"获取数据成功");

    }

    @GetMapping("/getScannerSumDifVoList")
    public ObjectRestResponse getScannerSumDifVoList(){

        return new ObjectRestResponse(true,this.baseBiz.getScannerSumDifVoList(),"获取数据成功");

    }

    @GetMapping("/weekDayCount")
    public ObjectRestResponse weekDayCount(){

        return new ObjectRestResponse(true,this.baseBiz.weekDayCount(),"获取数据成功");

    }
    @GetMapping("/mounthDayCount")
    public ObjectRestResponse mounthDayCount(){

        return new ObjectRestResponse(true,this.baseBiz.mounthDayCount(),"获取数据成功");

    }
    @GetMapping("/folibScannerGetOne")
    public ObjectRestResponse folibScannerGetOne(@RequestParam("id") String id){
        FolibScanner folibScanner = this.baseBiz.selectById(id);
        JSONArray jsonArray = JSON.parseArray(folibScanner.getReport());
       return new ObjectRestResponse<>(true,jsonArray,"成功") ;

    }


}
