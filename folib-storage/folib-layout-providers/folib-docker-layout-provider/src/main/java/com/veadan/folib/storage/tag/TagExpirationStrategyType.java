package com.veadan.folib.storage.tag;

import com.veadan.folib.api.Describable;

import java.util.stream.Stream;

/**
 * @author veadan
 * @date 2024/1/19
 **/
public enum TagExpirationStrategyType
        implements Describable {
    /**
     * checksum
     */
    CHECKSUM("checksum"),
    /**
     * refresh
     */
    REFRESH("refresh");

    private String strategy;

    TagExpirationStrategyType(String strategy) {
        this.strategy = strategy;
    }

    @Override
    public String describe() {
        return strategy;
    }

    public static TagExpirationStrategyType ofStrategy(String strategy) {
        return Stream.of(values())
                .filter(e -> e.strategy.equals(strategy))
                .findFirst()
                .orElse(null);
    }
}
