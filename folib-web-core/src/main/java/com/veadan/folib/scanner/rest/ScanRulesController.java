package com.veadan.folib.scanner.rest;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.scanner.biz.ScanRulesBiz;
import com.veadan.folib.scanner.common.base.BaseController;
import com.veadan.folib.scanner.common.msg.ObjectRestResponse;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.service.ScanRulesService;
import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/scanRules")
@Api(tags = "规则扫描控制器", description = "规则扫描控制器")
public class ScanRulesController extends BaseController<ScanRulesBiz, ScanRules, String> {

    @Autowired
    private ScanRulesService scanRulesService;

    @PostMapping("/insertOrUpdate")
    @AuditLog(value = AuditEventNameEnum.SCAN_ARTIfFACT,target ="#scanRules.storage + '-'+ #scanRules.repository" )
    public ObjectRestResponse insertOrUpdate(@RequestBody ScanRules scanRules) {
        scanRulesService.saveOrUpdateScanRules(scanRules);
        return new ObjectRestResponse(true, "更新成功");
    }

    @GetMapping("/queryBomOnScan")
    public ResponseEntity queryBomOnScan() {
        return ResponseEntity.ok(scanRulesService.queryBomOnScanList());
    }

    @GetMapping("/queryBomOnScanTree")
    public ResponseEntity queryBomOnScanTree() {
        return ResponseEntity.ok(scanRulesService.queryBomOnScanTree());
    }

    @GetMapping("/queryOnScanTree")
    public ResponseEntity queryOnScanTree() {
        return ResponseEntity.ok(scanRulesService.queryOnScanTree());
    }
}
