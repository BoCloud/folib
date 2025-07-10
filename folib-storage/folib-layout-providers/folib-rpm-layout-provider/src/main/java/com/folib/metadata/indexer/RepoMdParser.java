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
package com.folib.metadata.indexer;

import lombok.Getter;
import lombok.Setter;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.File;
import java.util.HashMap;
import java.util.Map;

public class RepoMdParser extends DefaultHandler {

    private Map<String, String> hrefs = new HashMap<>();
    private String currentDataType = null;


    public void parse(String filePath) throws Exception {
        SAXParserFactory factory = SAXParserFactory.newInstance();
        SAXParser saxParser = factory.newSAXParser();
        saxParser.parse(new File(filePath), this);
    }

    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        if ("data".equals(qName)) {
            // 获取"data"元素的type属性
            currentDataType = attributes.getValue("type");
        } else if ("location".equals(qName) && currentDataType != null) {
            // 获取"location"元素的href属性
            String href = attributes.getValue("href");
            if (href != null && (currentDataType.equals("primary") || currentDataType.equals("other") || currentDataType.equals("filelists"))) {
                hrefs.put(currentDataType, href);
            }
        }
    }

    @Override
    public void endElement(String uri, String localName, String qName) throws SAXException {
        if ("data".equals(qName)) {
            // 结束"data"元素时重置currentDataType
            currentDataType = null;
        }
    }

    public Map<String, String> getHrefs() {
        return hrefs;
    }
}
