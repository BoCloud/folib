package com.folib.index.processors;

import java.io.IOException;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.folib.index.MarkdownReader;
import com.folib.model.RevisionData;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;

public class LanguagesProcessor implements LineProcessor {
    private static final String LANGUAGE_PREFIX = "language:";

    private static final Pattern LANGUAGE_PATTERN_ONE_LINE = Pattern.compile("language:\\s*\\[([^\\]]+)\\]");

    public void process(@NonNull String line, @NonNull RevisionData revisionData, @NonNull MarkdownReader markdownReader) throws IOException {
        if (line == null)
            throw new NullPointerException("line is marked non-null but is null");
        if (revisionData == null)
            throw new NullPointerException("revisionData is marked non-null but is null");
        if (markdownReader == null)
            throw new NullPointerException("markdownReader is marked non-null but is null");
        Matcher langsListMatcher = LANGUAGE_PATTERN_ONE_LINE.matcher(line);
        if (langsListMatcher.matches()) {
            String langsWithQuotes = langsListMatcher.group(1);
            revisionData.getCardData().setLanguage(new ArrayList());
            String[] languages = langsWithQuotes.split(",");
            for (String language : languages) {
                if (StringUtils.isNoneEmpty(new CharSequence[] { language }))
                    revisionData.getCardData().getLanguage().add(language.trim().replace("\"", ""));
            }
        } else if (line.startsWith("language:")) {
            revisionData.getCardData().setLanguage(new ArrayList());
            String listItem;
            while ((listItem = markdownReader.readListItem()) != null) {
                String language = listItem.substring("- ".length()).trim();
                revisionData.getCardData().getLanguage().add(language);
            }
        }
    }
}
