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


import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;

import java.util.LinkedList;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author veadan
 * @since 2024-09-04 16:11
 */
@Slf4j
public class DebianIndexIncrementalFilter implements DeltaBasedIndexFilter {
    static final String FILENAME = "Filename:";
    private static final Pattern FILENAME_FIELD = Pattern.compile("^Filename:\\s*(.*)");
    private LinkedList<String> addEvents = new LinkedList();
    private Set<String> deleteEvents;

    DebianIndexIncrementalFilter(Set<String> addEvents, Set<String> deleteEvents) {
        this.addEvents.addAll(addEvents);
        this.deleteEvents = deleteEvents;
    }

    public boolean hasNextAddBlock() {
        return !this.addEvents.isEmpty();
    }

    public String getNextAddBlock() {
        String nextBlock;
        for (nextBlock = this.addEvents.pollFirst(); StringUtils.isBlank(nextBlock) && !this.addEvents.isEmpty(); nextBlock = this.addEvents.pollFirst()) {
        }
        return nextBlock.trim();
    }

    public boolean hasPendingRemovals() {
        return !this.deleteEvents.isEmpty();
    }

    public boolean shouldRemoveBlock(String line) {
        if (line != null) {
            log.debug("Testing line: {} ", line);
            Matcher filenameMatcher = FILENAME_FIELD.matcher(line);
            String warn;
            if (filenameMatcher.find()) {
                warn = filenameMatcher.group(1).trim();
                // 包含目录
                for (String deleteEvent : deleteEvents) {
                    if(warn.startsWith(deleteEvent)){
                        return true;
                    }
                }
//                if (this.deleteEvents.contains(warn)) {
//                    log.debug("Found delete event in filter for path {}", warn);
//                    return true;
//                }
            } else if (StringUtils.isNotBlank(line)) {
                log.warn("Given line does not contain expected field 'Filename', cannot process it.");
            }
        }
        return false;
    }

    public String getLineIdentifier() {
        return FILENAME;
    }
}
