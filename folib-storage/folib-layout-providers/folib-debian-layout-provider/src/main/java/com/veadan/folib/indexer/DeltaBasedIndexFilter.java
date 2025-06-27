package com.veadan.folib.indexer;

/**
 * @author veadan
 * @since 2024-09-03 14:42
 */
public interface DeltaBasedIndexFilter {
    boolean hasNextAddBlock();

    String getNextAddBlock();

    boolean hasPendingRemovals();

    boolean shouldRemoveBlock(String line);

    String getLineIdentifier();
}
