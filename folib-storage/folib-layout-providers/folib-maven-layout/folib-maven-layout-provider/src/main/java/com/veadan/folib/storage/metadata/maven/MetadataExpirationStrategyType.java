package com.veadan.folib.storage.metadata.maven;

import com.veadan.folib.api.Describable;

import java.util.stream.Stream;

public enum MetadataExpirationStrategyType
        implements Describable
{
    CHECKSUM("checksum"),
    REFRESH("refresh");

    private String strategy;

    MetadataExpirationStrategyType(String strategy)
    {
        this.strategy = strategy;
    }

    @Override
    public String describe()
    {
        return strategy;
    }

    public static MetadataExpirationStrategyType ofStrategy(String strategy)
    {
        return Stream.of(values())
                     .filter(e -> e.strategy.equals(strategy))
                     .findFirst()
                     .orElse(null);
    }
}
