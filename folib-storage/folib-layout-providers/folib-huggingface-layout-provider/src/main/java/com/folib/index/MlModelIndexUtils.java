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
package com.folib.index;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;

import com.folib.index.processors.*;
import com.folib.model.CardData;
import com.folib.model.RevisionData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class MlModelIndexUtils {

    private static final Logger log = LoggerFactory.getLogger(MlModelIndexUtils.class);

    public static final int MAX_STREAM_SIZE = 52428800;

    private MlModelIndexUtils() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    private static final List<LineProcessor> processors = List.of(new LicenseProcessor(), new LibraryNameProcessor(), new ModelNameProcessor(), new TagsProcessor(), new LanguagesProcessor());

    public static RevisionData parseReadme(InputStream stream) throws IOException {
        RevisionData ret = new RevisionData();
        ret.setCardData(new CardData());
        BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(stream));
        validateStreamNotExceedingSize(bufferedReader);
        MarkdownReader markdownReader = new MarkdownReader(bufferedReader);
        String line;
        while ((line = markdownReader.readLine()) != null) {
            for (LineProcessor processor : processors) {
                processor.process(line, ret, markdownReader);
            }
        }
        return ret;
    }

    private static void validateStreamNotExceedingSize(BufferedReader bis) throws RuntimeException, IOException {
        bis.mark(104857600);
        long totalBytesRead = 0L;
        char[] buffer = new char[1024];
        int bytesRead;
        while ((bytesRead = bis.read(buffer)) != -1) {
            totalBytesRead += bytesRead;
            if (totalBytesRead > 52428800L) {
                throw new RuntimeException("README size exceeds the maximum allowed of 50MB");
            }
        }
        bis.reset();
    }
}

