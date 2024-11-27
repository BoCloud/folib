package com.veadan.folib.index.processors;

import com.veadan.folib.index.MarkdownReader;
import com.veadan.folib.model.RevisionData;

import java.io.IOException;

public interface LineProcessor {
    void process( String paramString,  RevisionData paramRevisionData,  MarkdownReader paramMarkdownReader) throws IOException;
}
