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

import com.folib.metadata.indexer.xml.JsonToXmlConverter;
import com.folib.metadata.indexer.xml.Package;
import com.folib.metadata.indexer.xml.XmlParser;
import com.folib.providers.io.RepositoryPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;


public class PrimaryXmlMerger {

    private static final Logger logger = LoggerFactory.getLogger(PrimaryXmlMerger.class);

    public void mergePrimaryXmlFiles(List<RepositoryPath> xmlFilePaths, String savePath) throws Exception {
        XmlParser parser = new XmlParser();
        Set<Package> packageSet = new HashSet<>(); // 使用 HashSet 去重
        try {
            for (RepositoryPath filePath : xmlFilePaths) {
                List<Package> packages = parser.parse(filePath).getPackageList();
                packageSet.addAll(packages); // 添加到 HashSet 中自动去重
            }

            List<Package> packages = new ArrayList<>(packageSet); // 转换回 List

            // 调用 JsonToXmlConverter 转换为 XML 文件
            JsonToXmlConverter converter = new JsonToXmlConverter();
            converter.jsonToXml(packages, savePath + "/primary.xml");
            logger.info("Successfully merged primary XML files and saved to: {}", savePath + "/primary.xml");
        } catch (NoClassDefFoundError e) {
            logger.error("Failed to load JsonToXmlConverter class. Ensure the class is in the runtime classpath.", e);
            e.printStackTrace();
            throw e;

        } catch (Exception e) {
            e.printStackTrace();
            logger.error("An error occurred while merging primary XML files: {}", e.getMessage(), e);
            throw e;
        }
    }

}
