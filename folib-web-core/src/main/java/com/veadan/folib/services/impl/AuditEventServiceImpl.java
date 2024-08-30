package com.veadan.folib.services.impl;

import com.veadan.folib.entity.AuditEvent;
import com.veadan.folib.mapper.AuditEventMapper;
import com.veadan.folib.services.AuditEventService;
import org.springframework.stereotype.Service;
import tk.mybatis.mapper.entity.Example;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author huayanjun
 * @since 2024-08-13 16:54
 */
@Service
public class AuditEventServiceImpl implements AuditEventService {

    @Resource
    private AuditEventMapper auditEventMapper;

    @Override
    public AuditEvent findUsedEventByName(String eventValue) {
        Example example = Example.builder(AuditEvent.class).build();
        Example.Criteria where = example.createCriteria();
        where.andEqualTo("used", 1);
        where.andEqualTo("eventValue", eventValue);
        return auditEventMapper.selectOneByExample(example);
    }

    @Override
    public List<AuditEvent> findByModuleName(String moduleValue) {
        Example example = Example.builder(AuditEvent.class).build();
        Example.Criteria where = example.createCriteria();
        where.andEqualTo("used", 1);
        where.andEqualTo("moduleValue", moduleValue);
        return auditEventMapper.selectByExample(example);

    }

    @Override
    public List<AuditEvent> findAllModule() {
        return auditEventMapper.findAllModule();
    }

    @Override
    public List<AuditEvent> findAll() {
        return auditEventMapper.selectAll();
    }

    @Override
    public boolean updateById(AuditEvent event) {
        return auditEventMapper.updateById(event);
    }
}
