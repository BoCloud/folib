package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.entity.AuditLogRecord;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.dto.audit.AuditLogDto;
import com.veadan.folib.mapper.AuditLogRecordMapper;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.AuditEventService;
import com.veadan.folib.services.AuditLogRecordService;
import com.veadan.folib.utils.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.context.expression.MethodBasedEvaluationContext;
import org.springframework.core.DefaultParameterNameDiscoverer;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RequestBody;

import javax.annotation.Resource;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.List;

/**
 * @author veadan
 * @since 2024-08-12 16:13
 */
@Slf4j
@Service
public class AuditLogRecordServiceImpl implements AuditLogRecordService {

    @Resource
    private AuditLogRecordMapper auditLogRecordMapper;


    @Resource
    private AuditEventService auditEventService;

    @Override
    public boolean saveRecord(AuditLogRecord record) {
        int targetMaxLength = 255;
        String target = record.getTarget();
        if (StringUtils.isNotBlank(target) && target.length() >= targetMaxLength) {
            record.setTarget(target.substring(0, targetMaxLength));
        }
        return auditLogRecordMapper.insert(record) == 1;
    }

    private final ExpressionParser parser = new SpelExpressionParser();
    private final DefaultParameterNameDiscoverer nameDiscoverer = new DefaultParameterNameDiscoverer();

    @Async
    @Override
    public void recordLog(ProceedingJoinPoint joinPoint, AuditLog audit, Object result) {
        try {
            AuditEventNameEnum name = audit.value();
            if (auditEventService.findUsedEventByName(name.toString()) == null) {
                return;
            }
            MethodSignature signature = (MethodSignature) joinPoint.getSignature();
            Method method = signature.getMethod();
            String requestBody = captureRequestBody(method, joinPoint.getArgs());
            AuditLogRecord record = new AuditLogRecord();
            record.setRequest(requestBody);
            String username = UserUtils.getUsername();
            record.setUsername(username);
            record.setName(name.toString());
            record.setEventName(name.getName());
            record.setModule(name.getModule().toString());
            record.setModuleName(name.getModule().getName());

            record.setResponse(JSON.toJSONString(result));
            String target = parseSpEL(audit.target(), method, joinPoint.getArgs());
            record.setTarget(target);
            if (result instanceof ResponseEntity) {
                @SuppressWarnings("all")
                ResponseEntity entity = (ResponseEntity) result;
                if (entity.getStatusCode().is2xxSuccessful()) {
                    record.setResult(1);
                } else {
                    record.setResult(0);
                }
            } else {
                record.setResult(0);
            }
            saveRecord(record);
        } catch (Exception e) {
            log.error("审计日志记录失败", e);
        }
    }

    @Override
    public TableResultResponse<AuditLogRecord> page(AuditLogDto model) {
        PageHelper.startPage(model.getPageNumber(), model.getPageSize());
        List<AuditLogRecord> records = auditLogRecordMapper.selectList(Wrappers.<AuditLogRecord>lambdaQuery()
                .eq(StringUtils.isNotBlank(model.getModuleValue()),  AuditLogRecord::getModule, model.getModuleValue())
                .eq(StringUtils.isNotBlank(model.getEventValue()), AuditLogRecord::getName, model.getEventValue())
                .ge(model.getFromDate() != null, AuditLogRecord::getCreateTime, model.getFromDate())
                .le(model.getToDate() != null, AuditLogRecord::getCreateTime, model.getToDate())
                .orderByDesc(AuditLogRecord::getCreateTime)
        );

        PageInfo<AuditLogRecord> pageInfo = new PageInfo<>(records);
        return new TableResultResponse<>(pageInfo.getTotal(), records);
    }


    private String parseSpEL(String expression, Method method, Object[] args) {
        EvaluationContext context = new MethodBasedEvaluationContext(null, method, args, nameDiscoverer);
        // 添加方法参数到上下文
        String[] parameterNames = nameDiscoverer.getParameterNames(method);
        if (parameterNames != null) {
            for (int i = 0; i < parameterNames.length; i++) {
                context.setVariable(parameterNames[i], args[i]);
            }
        }
        return (String) parser.parseExpression(expression).getValue(context);

    }

    private String captureRequestBody(Method method, Object[] args) {
        Parameter[] parameters = method.getParameters();
        for (int i = 0; i < parameters.length; i++) {
            if (parameters[i].isAnnotationPresent(RequestBody.class)) {
                try {
                    return JSON.toJSONString(args[i]);
                } catch (Exception e) {
                    return "Error serializing request body: " + e.getMessage();
                }
            }
        }
        return "{}";
    }

}
