package com.folib.index.processors;

import java.io.IOException;

import com.folib.index.MarkdownReader;
import com.folib.model.RevisionData;

public class LicenseProcessor implements LineProcessor {
    private static final String LICENSE_LINE_PREFIX = "license:";

    public void process( String line,  RevisionData revisionData,  MarkdownReader markdownReader) throws IOException {
        if (line == null) {
            throw new NullPointerException("line is marked non-null but is null");
        }
        if (revisionData == null) {
            throw new NullPointerException("revisionData is marked non-null but is null");
        }
        if (markdownReader == null) {
            throw new NullPointerException("markdownReader is marked non-null but is null");
        }
        if (line.startsWith("license:")) {
            String license = line.substring("license:".length()).trim();
            if (!license.isEmpty()) {
                revisionData.getCardData().setLicense(license);
            } else {
                String listItem;
                while ((listItem = markdownReader.readListItem()) != null) {
                    revisionData.getCardData().setLicense(listItem.substring("- ".length()).trim());
                }
            }
        }
    }
}

