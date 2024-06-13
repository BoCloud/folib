package com.veadan.folib.domain.huggingface.index.processors;

import java.io.IOException;
import java.util.ArrayList;

import com.veadan.folib.domain.huggingface.index.MarkdownReader;
import com.veadan.folib.domain.huggingface.model.RevisionData;
import lombok.NonNull;

public class TagsProcessor implements LineProcessor {
    private static final String TAGS_PREFIX = "tags:";

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
        if (line.startsWith("tags:")) {
            revisionData.getCardData().setTags(new ArrayList());
            String listItem;
            while ((listItem = markdownReader.readListItem()) != null) {
                String tag = listItem.substring("- ".length()).trim();
                revisionData.getCardData().getTags().add(tag);
            }
        }
    }
}
