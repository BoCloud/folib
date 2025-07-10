/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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

