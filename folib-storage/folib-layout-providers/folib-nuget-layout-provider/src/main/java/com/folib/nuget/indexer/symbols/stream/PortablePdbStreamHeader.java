package com.folib.nuget.indexer.symbols.stream;

import lombok.Getter;


@Getter
public class PortablePdbStreamHeader {
    public static final int STREAM_NAME_MAX_SIZE = 32;
    private final int streamOffset;
    private final int streamSize;
    private final String streamName;

    public PortablePdbStreamHeader(int streamOffset, int streamSize, String streamName) {
        this.streamOffset = streamOffset;
        this.streamSize = streamSize;
        this.streamName = streamName;
    }
}
