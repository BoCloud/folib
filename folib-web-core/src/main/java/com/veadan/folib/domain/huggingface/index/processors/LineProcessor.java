package com.veadan.folib.domain.huggingface.index.processors;

import com.veadan.folib.domain.huggingface.index.MarkdownReader;
import com.veadan.folib.domain.huggingface.model.RevisionData;

import java.io.IOException;

public interface LineProcessor {
    void process( String paramString,  RevisionData paramRevisionData,  MarkdownReader paramMarkdownReader) throws IOException;
}
