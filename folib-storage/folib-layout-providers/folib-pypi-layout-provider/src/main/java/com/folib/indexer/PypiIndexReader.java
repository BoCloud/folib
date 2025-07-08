package com.folib.indexer;

/**
 * @author veadan
 * @date 2024/7/3
 **/

import com.google.common.collect.Sets;
import com.folib.model.PypiIndexEntry;
import com.folib.model.PypiSimpleIndex;
import lombok.Generated;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public class PypiIndexReader {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(PypiIndexReader.class);

    public static final String DATA_REQUIRES_PYTHON = "data-requires-python";

    public static final String DATA_YANKED = "data-yanked";

    public static final String DATA_DIST_INFO_METADATA = "data-dist-info-metadata";

    public static final String DATA_CORE_METADATA = "data-core-metadata";

    public static PypiSimpleIndex read(InputStream inputStream) throws IOException {
        if (inputStream == null) {
            return null;
        }
        Document doc = parseXMLDocument(inputStream);
        PypiSimpleIndex index = new PypiSimpleIndex(Sets.newTreeSet());
        Elements pypiEntries = doc.select("a");
        pypiEntries.forEach(entry -> {
            PypiIndexEntry indexEntry = xmlNodeToPypiIndexEntry(entry);
            if (indexEntry == null) {
                log.trace("Couldn't extract name/link attribute from line:{}, Ignoring line", entry.html());
            } else {
                log.trace("Index entry: {}", indexEntry);
                index.getEntries().add(indexEntry);
            }
        });
        return index;
    }

    @Nullable
    private static PypiIndexEntry xmlNodeToPypiIndexEntry(@NonNull Element entry) {
        if (entry == null) {
            throw new NullPointerException("entry is marked non-null but is null");
        }
        String name = entry.text();
        String link = entry.attr("href");
        if (StringUtils.isEmpty(name) || StringUtils.isEmpty(link)) {
            return null;
        }
        return new PypiIndexEntry(name, link);
    }

    @NonNull
    private static Document parseXMLDocument(@NonNull InputStream inputStream) throws IOException {
        if (inputStream == null)
            throw new NullPointerException("inputStream is marked non-null but is null");
        return Jsoup.parse(inputStream, StandardCharsets.UTF_8.toString(), "");
    }
}

