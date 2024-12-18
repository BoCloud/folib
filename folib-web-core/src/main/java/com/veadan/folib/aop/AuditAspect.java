package com.veadan.folib.aop;


import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.services.AuditLogRecordService;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * @author huayanjun
 * @since 2024-08-12 15:16
 */
@Slf4j
@Aspect
@Component
public class AuditAspect {

    @Resource
    private AuditLogRecordService auditLogRecordService;

    @Around("@annotation(audit)")
    public Object auditMethod(ProceedingJoinPoint joinPoint, AuditLog audit) throws Throwable {
        Object result = null;
        try {
            // 执行目标方法
            result = joinPoint.proceed();
            if (!AuditEventNameEnum.DOWNLOAD_EXCEPTION.equals(audit.value())) {
                // 正常情况下记录日志
                auditLogRecordService.recordLog(joinPoint, audit, result);
            }
        } catch (Throwable ex) {
            // 捕获异常时记录日志
            log.error("Audit failed for method: {}", joinPoint.getSignature(), ex);
            auditLogRecordService.recordLog(joinPoint, audit, ex);
            // 重新抛出异常，避免影响正常逻辑
            throw ex;
        }
        return result;
    }


}
