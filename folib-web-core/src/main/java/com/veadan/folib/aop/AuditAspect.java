package com.veadan.folib.aop;


import com.veadan.folib.annotation.AuditLog;
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
        Object result = joinPoint.proceed();
        auditLogRecordService.recordLog(joinPoint, audit, result);
        return result;
    }


}
