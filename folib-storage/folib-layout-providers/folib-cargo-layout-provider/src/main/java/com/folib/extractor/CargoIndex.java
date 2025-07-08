package com.folib.extractor;


import lombok.Getter;

public class CargoIndex {

    @Getter
    private final String packageName;

    @Getter
    private final String registry;

    @Getter
    private final EventType type;

    public String toString() {
        return "CargoIndexEvent(packageName=" + getPackageName() + ", type=" + getType() + ")";
    }


    public CargoIndex( String packageName, EventType type) {
        this.type = type;
        this.packageName = packageName;
        this.registry = "https://github.com/rust-lang/crates.io-index";
    }

    public CargoIndex( String packageName, EventType type,String registry) {
        this.type = type;
        this.packageName = packageName;
        this.registry = registry;
    }

    public enum EventType {
        ADD, CALCULATE_METADATA, YANK, DELETE, REINDEX;
    }
}
