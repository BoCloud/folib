package com.folib.index.processors;

import com.folib.index.MarkdownReader;
import com.folib.model.RevisionData;

import java.io.IOException;

public interface LineProcessor {
    void process( String paramString,  RevisionData paramRevisionData,  MarkdownReader paramMarkdownReader) throws IOException;
}
