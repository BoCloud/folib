package com.veadan.folib.event.privilege;

import com.veadan.folib.event.AbstractEventListenerRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class PrivilegeEventListenerRegistry extends AbstractEventListenerRegistry {

    private static final Logger logger = LoggerFactory.getLogger(PrivilegeEventListenerRegistry.class);

    public void dispatchUserSyncEvent(String uuId) {
        PrivilegeEvent event = new PrivilegeEvent(uuId,
                PrivilegeEventTypeEnum.EVENT_USER_SYNC.getType());

        logger.info("Dispatching PrivilegeEventTypeEnum.EVENT_USER_SYNC event for {}...", uuId);

        dispatchEvent(event);
    }

    public void dispatchUserGroupSyncEvent(String uuId) {
        PrivilegeEvent event = new PrivilegeEvent(uuId,
                PrivilegeEventTypeEnum.EVENT_USER_GROUP_SYNC.getType());

        logger.info("Dispatching PrivilegeEventTypeEnum.EVENT_USER_GROUP_SYNC event for {}...", uuId);

        dispatchEvent(event);
    }
    public void dispatchRoleSyncEvent(String uuId) {
        PrivilegeEvent event = new PrivilegeEvent(uuId,
                PrivilegeEventTypeEnum.EVENT_ROLE_SYNC.getType());

        logger.info("Dispatching PrivilegeEventTypeEnum.EVENT_ROLE_SYNC event for {}...", uuId);

        dispatchEvent(event);
    }
    public void dispatchResourceSyncEvent(String uuId) {
        PrivilegeEvent event = new PrivilegeEvent(uuId,
                PrivilegeEventTypeEnum.EVENT_RESOURCE_SYNC.getType());

        logger.info("Dispatching PrivilegeEventTypeEnum.EVENT_RESOURCE_SYNC event for {}...", uuId);

        dispatchEvent(event);
    }

}
