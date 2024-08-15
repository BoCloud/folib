package com.veadan.folib.event.privilege;

/**
 * @author Veadan
 */
public enum PrivilegeEventTypeEnum {

    /**
     * Occurs when an artifact directory has been deleted.
     */
    EVENT_USER_SYNC(1),
    /**
     * Occurs when an metadata operation has update.
     */
    EVENT_USER_GROUP_SYNC(2),
    /**
     * Occurs when an artifact download operation has blocked.
     */
    EVENT_ROLE_SYNC(3),
    /**
     * Occurs when an artifact cache operation has need.
     */
    EVENT_RESOURCE_SYNC(4),
    EVENT_ALL_SYNC(5),
    ;


    private int type;


    PrivilegeEventTypeEnum(int type) {
        this.type = type;
    }

    public int getType() {
        return type;
    }

    /**
     * 根据类型查询枚举类型
     *
     * @param type 类型
     * @return 枚举类型
     */
    public static PrivilegeEventTypeEnum queryPrivilegeEventTypeEnumByType(int type) {
        for (PrivilegeEventTypeEnum artifactEventTypeEnum : PrivilegeEventTypeEnum.values()) {
            if (artifactEventTypeEnum.getType() == type) {
                return artifactEventTypeEnum;
            }
        }
        return null;
    }
}
