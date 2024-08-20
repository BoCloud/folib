package com.veadan.folib.scanner.rest;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.scanner.biz.FolibScannerBiz;
import com.veadan.folib.scanner.common.base.BaseController;
import com.veadan.folib.scanner.common.msg.ObjectRestResponse;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.scanner.entity.FolibScanner;
import com.veadan.folib.scanner.entity.FolibScannerDockerTableVO;
import com.veadan.folib.scanner.entity.SeverityVO;
import com.veadan.folib.scanner.service.ScanService;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
@RequestMapping("/api/folibScanner")
@Api(tags = "")
public class FolibScannerController extends BaseController<FolibScannerBiz, FolibScanner, String> {

    @Autowired
    private ScanService scanService;

    @GetMapping("/update")
    @PreAuthorize("hasAuthority('ADMIN')")
    public ObjectRestResponse updateDb(String cron) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
        scanService.vulnerabilityRefreshData(userDetails.getUsername(), cron);
        return new ObjectRestResponse(true, "更新中");
    }

    @GetMapping("/getCount")
    public ObjectRestResponse getCount() {
        JSONObject object = new JSONObject();
        object.put("denpendencyCount", this.baseBiz.getScanSum());
        object.put("totalCount", this.baseBiz.getTotalSum());
        return new ObjectRestResponse(true, object, "获取数据成功");
    }

    @GetMapping("/getScannerSumDifVoList")
    public ObjectRestResponse getScannerSumDifVoList() {
        return new ObjectRestResponse(true, this.baseBiz.getScannerSumDifVoList(), "获取数据成功");
    }

    @GetMapping("/weekDayCount")
    public ObjectRestResponse weekDayCount() {
        return new ObjectRestResponse(true, this.baseBiz.weekDayCount(), "获取数据成功");
    }

    @GetMapping("/mounthDayCount")
    public ObjectRestResponse mounthDayCount() {
        return new ObjectRestResponse(true, this.baseBiz.mounthDayCount(), "获取数据成功");
    }

    @GetMapping("/folibScannerGetOne")
    public ObjectRestResponse folibScannerGetOne(@RequestParam("id") String id) {
        FolibScanner folibScanner = this.baseBiz.selectById(id);
        JSONArray jsonArray = JSON.parseArray(folibScanner.getReport());
        return new ObjectRestResponse<>(true, jsonArray, "成功");
    }

    /**
     * 获取制品的漏洞严重程度信息
     *
     * @param id    制品id
     * @param fuzzy 模糊匹配 0 否 1 是
     * @return 制品的漏洞严重程度信息
     */
    @GetMapping("/severity")
    public ObjectRestResponse<SeverityVO> severity(@RequestParam("id") String id, @RequestParam(name = "fuzzy", required = false, defaultValue = "0") Integer fuzzy) {
        return new ObjectRestResponse<SeverityVO>(true, this.baseBiz.severity(id, fuzzy), "成功");
    }

    /**
     * 查询docker布局扫描报告
     *
     * @param params 参数
     * @return docker布局扫描报告
     */
    @GetMapping(value = "/dockerPage")
    public TableResultResponse<FolibScannerDockerTableVO> dockerPage(@RequestParam(required = false) Map<String, Object> params) {
        return this.baseBiz.dockerPage(params);
    }

}
