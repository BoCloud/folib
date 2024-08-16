package com.veadan.folib.mapper;

import com.veadan.folib.entity.AuditEvent;
import com.veadan.folib.scanner.common.base.CommonMapper;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author huayanjun
 * @since 2024-08-13 16:57
 */
@Component
public interface AuditEventMapper extends CommonMapper<AuditEvent> {

    boolean updateById(AuditEvent event);

    List<AuditEvent> findAllModule();
}
