package com.folib.nuget.indexer.symbols.stream;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public class MsfStream {
    private final int streamLength;
    private final List<Integer> pageList = new ArrayList();

    public MsfStream(int streamLength) {
        this.streamLength = streamLength;
    }

    public void addPage(int page) {
        this.pageList.add(page);
    }
}
