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

import com.folib.providers.io.RepositoryPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.BufferedInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.GZIPInputStream;

public class FileListsXmlMerger {

    private static final Logger logger = LoggerFactory.getLogger(FileListsXmlMerger.class);
    private Map<String, Package> packages = new HashMap<>();


    public void aggregate(Path filePath) throws Exception {
        logger.info("FileListsXmlMerger aggregate file: " + filePath);
        if(!Files.exists(filePath)){
            logger.warn("FileListsXmlMerger aggregate file not exists: " + filePath);
            return;
        }
        // 创建SAX解析器工厂实例
        SAXParserFactory factory = SAXParserFactory.newInstance();
        SAXParser saxParser = factory.newSAXParser();

        // 获取XML阅读器
        XMLReader xmlReader = saxParser.getXMLReader();

        // 设置自定义的DefaultHandler（您的处理逻辑）
        DefaultHandler handler = new PackageHandler(packages);
        xmlReader.setContentHandler(handler);

        // 打开GZIP输入流
        try (GZIPInputStream gzipInputStream = new GZIPInputStream(new BufferedInputStream(Files.newInputStream(filePath)))) {
            // 创建InputSource并设置字符编码
            InputSource inputSource = new InputSource(gzipInputStream);
            inputSource.setEncoding("UTF-8");

            // 使用XML阅读器解析输入源
            xmlReader.parse(inputSource);
        }
    }

    public void writeToFile(Path outputPath) throws Exception {
        logger.info("FileListsXmlMerger writeToFile outputPath: " + outputPath);
        DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
        Document doc = docBuilder.newDocument();

        Element rootElement = doc.createElement("filelists");
        rootElement.setAttribute("packages", String.valueOf(packages.size()));
        doc.appendChild(rootElement);

        for (Package pkg : packages.values()) {
            pkg.toXmlElement(doc, rootElement);
        }

        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        Transformer transformer = transformerFactory.newTransformer();
        transformer.setOutputProperty(OutputKeys.INDENT, "yes");
        DOMSource source = new DOMSource(doc);
        Files.createDirectories(outputPath.getParent());
        StreamResult result = new StreamResult(Files.newOutputStream(outputPath));
        transformer.transform(source, result);
    }

    public static class PackageHandler extends DefaultHandler {
        private Map<String, Package> packages;
        private Package currentPackage = null;
        private StringBuilder buffer = new StringBuilder();
        private String currentVersionVer = null;
        private String currentVersionRel = null;

        public PackageHandler(Map<String, Package> packages) {
            this.packages = packages;
        }

        @Override
        public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
            if ("package".equals(qName)) {
                currentPackage = new Package(attributes.getValue("pkgid"), attributes.getValue("name"), attributes.getValue("arch"));
            } else if ("version".equals(qName)) {
                currentVersionVer = attributes.getValue("ver");
                currentVersionRel = attributes.getValue("rel");
            }
            buffer.setLength(0); // 清空buffer
        }

        @Override
        public void characters(char[] ch, int start, int length) throws SAXException {
            buffer.append(ch, start, length);
        }

        @Override
        public void endElement(String uri, String localName, String qName) throws SAXException {
            if (currentPackage != null) {
                if ("version".equals(qName)) {
                    currentPackage.setVersion(currentVersionVer, currentVersionRel);
                } else if ("file".equals(qName)) {
                    currentPackage.addFile(buffer.toString());
                } else if ("package".equals(qName)) {
                    packages.putIfAbsent(currentPackage.getPkgId(), currentPackage);
                    currentPackage = null;
                }
            }
        }
    }

    public static class Package {
        private String pkgId, name, arch, version, release;
        private final List<String> files = new ArrayList<>();

        public Package(String pkgId, String name, String arch) {
            this.pkgId = pkgId;
            this.name = name;
            this.arch = arch;
        }

        public void setVersion(String ver, String rel) {
            this.version = ver;
            this.release = rel;
        }

        public void addFile(String file) {
            files.add(file);
        }

        public String getPkgId() {
            return pkgId;
        }

        public void toXmlElement(Document doc, Element rootElement) {
            Element packageElement = doc.createElement("package");
            packageElement.setAttribute("pkgid", pkgId);
            packageElement.setAttribute("name", name);
            packageElement.setAttribute("arch", arch);
            Element versionElement = doc.createElement("version");
            versionElement.setAttribute("ver", version);
            versionElement.setAttribute("rel", release);
            packageElement.appendChild(versionElement);
            for (String file : files) {
                Element fileElement = doc.createElement("file");
                fileElement.setTextContent(file);
                packageElement.appendChild(fileElement);
            }
            rootElement.appendChild(packageElement);
        }
    }

    /**
     * 合并多个filelists.xml文件
     * @param xmlFilePaths 文件路径集合
     * @param savePath
     * @throws Exception
     */
    public void mergeFileListsXmlFiles(List<RepositoryPath> xmlFilePaths, String savePath) throws Exception {
        for (RepositoryPath filePath : xmlFilePaths) {
            aggregate(filePath);
        }
        writeToFile(Paths.get(savePath, "filelists.xml"));
    }
}
