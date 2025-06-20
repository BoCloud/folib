package com.veadan.folib.constant;

public final class SystemProperty<T> {
    private final String name;

    private final T defaultValue;

    public SystemProperty(String name, T defaultValue) {
        this.name = name;
        this.defaultValue = defaultValue;
    }


    public String name() {
        return this.name;
    }

    public T defaultValue() {
        return this.defaultValue;
    }

    public static <T> SystemProperty<T> of(String name, T defaultValue) {
        return new SystemProperty<>(name, defaultValue);
    }
}
