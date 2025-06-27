package com.veadan.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.veadan.folib.entity.AuditEvent;
import com.veadan.folib.mapper.AuditEventMapper;
import com.veadan.folib.services.AuditEventService;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author veadan
 * @since 2024-08-13 16:54
 */
@Service
public class AuditEventServiceImpl implements AuditEventService {

    @Resource
    private AuditEventMapper auditEventMapper;

    @Override
    public AuditEvent findUsedEventByName(String eventValue) {
       return auditEventMapper.selectOne(Wrappers.<AuditEvent>lambdaQuery()
                .eq(AuditEvent::getUsed, 1)
                .eq(AuditEvent::getEventValue, eventValue)
        );
    }

    @Override
    public List<AuditEvent> findByModuleName(String moduleValue) {

       return auditEventMapper.selectList(Wrappers.<AuditEvent>lambdaQuery()
                .eq(AuditEvent::getUsed, 1)
                .eq(AuditEvent::getModuleValue, moduleValue)
        );

    }

    @Override
    public List<AuditEvent> findAllModule() {
        return auditEventMapper.findAllModule();
    }

    @Override
    public List<AuditEvent> findAll() {
        return  auditEventMapper.selectList(Wrappers.<AuditEvent>lambdaQuery());
    }

    @Override
    public boolean updateById(AuditEvent event) {
        return auditEventMapper.updateById(event) == 1;
    }
}
