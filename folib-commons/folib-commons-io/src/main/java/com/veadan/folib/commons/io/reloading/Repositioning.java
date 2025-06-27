package com.veadan.folib.commons.io.reloading;

import java.io.IOException;

/**
 * @author veadan
 */
public interface Repositioning
{

    /**
     * Reposition automatically based on the list of byte ranges.
     *
     * @throws IOException
     */
    void reposition() throws IOException;

    /**
     * Reposition manually.
     *
     * @param skipBytes
     */
    void reposition(long skipBytes);

    boolean hasMoreByteRanges();

}
