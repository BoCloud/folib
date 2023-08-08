package com.veadan.folib.scanner.rest;

import com.veadan.folib.scanner.biz.ScanRulesBiz;
import com.veadan.folib.scanner.common.base.BaseController;
import com.veadan.folib.scanner.common.msg.ObjectRestResponse;
import com.veadan.folib.scanner.entity.ScanRules;
import io.swagger.annotations.Api;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/scanRules")
@Api(tags = "规则扫描控制器",description = "规则扫描控制器")
public class ScanRulesController extends BaseController<ScanRulesBiz, ScanRules, String> {


    @PostMapping("/insertOrUpdate")
    public ObjectRestResponse insertOrUpdate(@RequestBody ScanRules entity) {
        ScanRules q = this.baseBiz.selectById(entity.getId());
        if (q != null) {
            this.baseBiz.updateSelectiveById(entity);
        } else {
            this.baseBiz.insertSelective(entity);
        }
        return new ObjectRestResponse(true, "更新成功");
    }
}
