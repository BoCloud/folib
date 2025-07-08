package com.folib.scanner.enums;

import java.util.Arrays;

public enum Severity {
    CRITICAL (5),
    HIGH (4),
    MEDIUM (3),
    LOW (2),
    INFO (1),
    UNASSIGNED (0);

    private final int level;

    Severity(final int level) {
        this.level = level;
    }

    public int getLevel() {
        return level;
    }

    public static Severity getSeverityByLevel(final int level){
        return Arrays.stream(values()).filter(value -> value.level == level).findFirst().orElse(UNASSIGNED);
    }
}
