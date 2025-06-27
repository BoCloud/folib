package com.veadan.folib.services;

import com.veadan.folib.entity.AuditEvent;

import java.util.List;

/**
 * @author veadan
 * @since 2024-08-13 16:54
 */
public interface AuditEventService {

    AuditEvent findUsedEventByName(String eventName);

    List<AuditEvent> findByModuleName(String moduleName);

    List<AuditEvent> findAllModule();


    List<AuditEvent> findAll();

    boolean updateById(AuditEvent event);
}
