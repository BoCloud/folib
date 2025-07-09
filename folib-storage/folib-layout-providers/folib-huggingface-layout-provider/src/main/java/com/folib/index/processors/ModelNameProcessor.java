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

