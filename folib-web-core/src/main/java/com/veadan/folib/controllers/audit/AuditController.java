package com.veadan.folib.controllers.audit;

import com.veadan.folib.entity.AuditEvent;
import com.veadan.folib.entity.AuditLogRecord;
import com.veadan.folib.forms.audit.AuditLogForm;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.AuditEventService;
import com.veadan.folib.services.AuditLogRecordService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author veadan
 * @since 2024-08-14 15:44
 */

@RestController
@Api(tags = "审计日志")
@RequestMapping("/api/audit")
@PreAuthorize("hasAuthority('ADMIN')")
public class AuditController {

    @Resource
    private AuditLogRecordService auditLogRecordService;

    @Resource
    private AuditEventService auditEventService;


    @ApiOperation(value = "审计日志分页查询")
    @PostMapping(value = "/log")
    public TableResultResponse<AuditLogRecord> page(@RequestBody AuditLogForm model) {
        return auditLogRecordService.page(model);

    }

    @ApiOperation(value = "审计模块")
    @GetMapping(value = "/module")
    public ResponseEntity<List<AuditEvent>> module() {
        List<AuditEvent> modules = auditEventService.findAllModule();
        return ResponseEntity.ok(modules);

    }

    @ApiOperation(value = "审计事件")
    @GetMapping(value = "/event/{module}")
    public ResponseEntity<List<AuditEvent>> event(@PathVariable String module) {
        List<AuditEvent> events = auditEventService.findByModuleName(module);
        return ResponseEntity.ok(events);
    }

    @ApiOperation(value = "审计事件全量")
    @GetMapping(value = "/event")
    public ResponseEntity<List<AuditEvent>> event() {
        List<AuditEvent> events = auditEventService.findAll();
        return ResponseEntity.ok(events);
    }


    @ApiOperation(value = "审计事件")
    @PostMapping(value = "/event")
    public ResponseEntity<String> updateEvent(@RequestBody AuditEvent event) {
        auditEventService.updateById(event);
        return ResponseEntity.ok("事件更新成功");
    }


}
