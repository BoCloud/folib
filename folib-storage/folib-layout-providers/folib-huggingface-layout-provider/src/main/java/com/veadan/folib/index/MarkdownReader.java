package com.veadan.folib.index;

import java.io.BufferedReader;
import java.io.IOException;
import javax.annotation.Nullable;

public class MarkdownReader extends BufferedReader {
    public static final String LIST_ITEM_PREFIX = "- ";

    public static final int LINE_LENGTH_LIMIT = 10000;

    private static final String METADATA_SEPARATOR = "---";

    protected static final int READ_AHEAD_LIMIT = 5000000;

    private int metadataSeparatorsMet = 0;

    public MarkdownReader(BufferedReader in) {
        super(in);
    }

    @Nullable
    public String readLine() throws IOException {
        String line = super.readLine();
        if (line != null && line.equals("---")) {
            this.metadataSeparatorsMet++;
            if (this.metadataSeparatorsMet == 2)
                return null;
            line = super.readLine();
        }
        return line;
    }

    @Nullable
    public String readListItem() throws IOException {
        mark(5000000);
        String line = super.readLine();
        if (line == null || !line.startsWith("- ")) {
            reset();
            line = null;
        }
        return line;
    }
}