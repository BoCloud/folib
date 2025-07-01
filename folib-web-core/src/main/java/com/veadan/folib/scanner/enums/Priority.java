package com.veadan.folib.scanner.enums;

/**
 * 队列优先级（值越小等级越高）
 */
public enum Priority {

    //置顶
    STICKY(0),
    //高
    HIGH(1),
    //中
    MEDIUM(2),
    //低
    LOW(3);

    private int value;

    Priority(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }
    public static Priority getPriority(int value) {
        for (Priority priority : Priority.values()) {
            if (priority.getValue() == value) {
                return priority;
            }
        }
        return null;
    }
}
