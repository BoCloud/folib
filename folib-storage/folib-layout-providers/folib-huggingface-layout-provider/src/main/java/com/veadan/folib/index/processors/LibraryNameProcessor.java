package com.veadan.folib.index.processors;

import com.veadan.folib.index.MarkdownReader;
import com.veadan.folib.model.RevisionData;

public class LibraryNameProcessor implements LineProcessor {
    private static final String LIBRARY_NAME_PREFIX = "library_name:";

    public void process( String line,  RevisionData revisionData,  MarkdownReader markdownReader) {
        if (line == null) {
            throw new NullPointerException("line is marked non-null but is null");
        }
        if (revisionData == null) {
            throw new NullPointerException("revisionData is marked non-null but is null");
        }
        if (markdownReader == null) {
            throw new NullPointerException("markdownReader is marked non-null but is null");
        }
        if (line.startsWith("library_name:")) {
            String libraryName = line.substring("library_name:".length()).trim();
            revisionData.setLibraryName(libraryName);
        }
    }
}
