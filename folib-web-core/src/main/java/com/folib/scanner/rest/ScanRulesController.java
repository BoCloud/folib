package com.folib.scanner.rest;

import com.folib.scanner.common.msg.ObjectRestResponse;
import com.folib.scanner.entity.ScanRules;
import com.folib.scanner.service.ScanRulesService;
import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/scanRules")
@Api(tags = "规则扫描控制器", description = "规则扫描控制器")
public class ScanRulesController  {

    @Autowired
    private ScanRulesService scanRulesService;

    @PostMapping("/insertOrUpdate")
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

    @GetMapping("/{scanId}")
    public ResponseEntity queryOnScan(@PathVariable("scanId") String scanId) {
        ObjectRestResponse<ScanRules> entityObjectRestResponse = new ObjectRestResponse<>();
        ScanRules rules = scanRulesService.findByScanId(scanId);
        entityObjectRestResponse.data(rules);
        entityObjectRestResponse.setRel(rules != null);
        return ResponseEntity.ok(entityObjectRestResponse);
    }
}
