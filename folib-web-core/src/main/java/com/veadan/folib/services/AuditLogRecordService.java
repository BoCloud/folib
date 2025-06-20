package com.veadan.folib.services;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.entity.AuditLogRecord;
import com.veadan.folib.dto.audit.AuditLogDto;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import org.aspectj.lang.ProceedingJoinPoint;

/**
 * @author huayanjun
 * @since 2024-08-12 16:12
 */
public interface AuditLogRecordService {
    boolean saveRecord(AuditLogRecord record);

    void recordLog(ProceedingJoinPoint joinPoint, AuditLog audit,Object result);

    TableResultResponse<AuditLogRecord> page(AuditLogDto model);
}
