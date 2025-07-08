package com.folib.index.processors;

import java.io.IOException;

import com.folib.index.MarkdownReader;
import com.folib.model.RevisionData;
import lombok.NonNull;

public class ModelNameProcessor implements LineProcessor {
    private static final String MODEL_INDEX_PREFIX = "model-index:";

    private static final String MODEL_NAME = "- name:";

    public void process(@NonNull String line, @NonNull RevisionData revisionData, @NonNull MarkdownReader markdownReader) throws IOException {
        if (line == null) {
            throw new NullPointerException("line is marked non-null but is null");
        }
        if (revisionData == null) {
            throw new NullPointerException("revisionData is marked non-null but is null");
        }
        if (markdownReader == null) {
            throw new NullPointerException("markdownReader is marked non-null but is null");
        }
        if (line.startsWith("model-index:")) {
            line = markdownReader.readListItem();
            if (line == null) {
                return;
            }
            if (line.startsWith("- name:")) {
                String modelName = line.substring("- name:".length()).trim();
                revisionData.setModelId(modelName);
            }
        }
    }
}

