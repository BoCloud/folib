package com.veadan.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.veadan.folib.entity.AuditEvent;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author veadan
 * @since 2024-08-13 16:57
 */
@Component
public interface AuditEventMapper extends BaseMapper<AuditEvent> {

    int updateById(AuditEvent event);

    List<AuditEvent> findAllModule();
}
