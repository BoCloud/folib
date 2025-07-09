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
package com.folib.metadata.indexer.xml;

import com.folib.providers.io.RepositoryPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.events.*;
import java.io.BufferedInputStream;

import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.zip.GZIPInputStream;

public class XmlParser {
    private static final Logger logger = LoggerFactory.getLogger(XmlParser.class);
    private static final String REPODATA_PREFIX = "repodata/";
    public Metadata parse(RepositoryPath filePath) throws Exception {
        logger.info("XmlParser parse filePath:{}",filePath);
        XMLInputFactory factory = XMLInputFactory.newInstance();
        // 安全配置防止XXE
        factory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false);
        factory.setProperty(XMLInputFactory.SUPPORT_DTD, false);
        String fullPath = filePath.getPath();
        String fileName = filePath.getFileName().toString();
        String locationBasePath = fullPath.startsWith(REPODATA_PREFIX + fileName) ? "" : fullPath.replace(REPODATA_PREFIX + fileName, "");

        // 打开GZIP输入流
        try (GZIPInputStream gzipInputStream = new GZIPInputStream(new BufferedInputStream(Files.newInputStream(filePath)))) {
            XMLEventReader reader = factory.createXMLEventReader(new InputStreamReader(gzipInputStream, StandardCharsets.UTF_8));
            Metadata metadata = new Metadata();
            Package currentPackage = null;
            Format currentFormat = null;
            Entry currentEntry = null;
            PackageFile currentFile = null;
            String currentElement = null;
            String currentParent = null; // 跟踪父元素（requires/provides/conflicts）
            HeaderRange currentHeaderRange = null;

            while (reader.hasNext()) {
                XMLEvent event = reader.nextEvent();

                if (event.isStartElement()) {
                    StartElement startElement = event.asStartElement();
                    String elementName = startElement.getName().getLocalPart();

                    switch (elementName) {
                        case "metadata":
                            metadata.setXmlns(getAttribute(startElement, "xmlns"));
                            metadata.setXmlnsRpm(getAttribute(startElement, "xmlns:rpm"));
                            metadata.setPackages(Integer.parseInt(getAttribute(startElement, "packages")));
                            metadata.setPackageList(new ArrayList<>());
                            break;

                        case "package":
                            currentPackage = new Package();
                            currentPackage.setType(getAttribute(startElement, "type"));
                            currentPackage.setFormat(new Format()); // 初始化 format
                            break;

                        case "name":
                            currentElement = "name";
                            break;

                        case "arch":
                            currentElement = "arch";
                            break;

                        case "version":
                            Version version = new Version();
                            version.setEpoch(getAttribute(startElement, "epoch"));
                            version.setVer(getAttribute(startElement, "ver"));
                            version.setRel(getAttribute(startElement, "rel"));
                            currentPackage.setVersion(version);
                            break;

                        case "checksum":
                            Checksum checksum = new Checksum();
                            checksum.setType(getAttribute(startElement, "type"));
                            checksum.setPkgid(getAttribute(startElement, "pkgid"));
                            currentPackage.setChecksum(checksum);
                            currentElement = "checksum";
                            break;

                        case "summary":
                            currentElement = "summary";
                            break;

                        case "description":
                            currentElement = "description";
                            break;

                        case "packager":
                            currentElement = "packager";
                            break;

                        case "url":
                            currentElement = "url";
                            break;
                        case "time":
                            Time time = new Time();
                            time.setFile(Long.parseLong(getAttribute(startElement, "file")));
                            time.setBuild(Long.parseLong(getAttribute(startElement, "build")));
                            currentPackage.setTime(time);
                            break;

                        case "size":
                            Size size = new Size();
                            size.setPackageSize(Long.parseLong(getAttribute(startElement, "package")));
                            size.setInstalled(Long.parseLong(getAttribute(startElement, "installed")));
                            size.setArchive(Long.parseLong(getAttribute(startElement, "archive")));
                            currentPackage.setSize(size);
                            break;

                        case "location":
                            Location location = new Location();
                            location.setHref(locationBasePath+ getAttribute(startElement, "href"));
                            currentPackage.setLocation(location);
                            break;

                        case "format":
                            currentFormat = currentPackage.getFormat();
                            currentFormat.setRequires(new ArrayList<>());
                            currentFormat.setProvides(new ArrayList<>());
                            currentFormat.setConflicts(new ArrayList<>());
                            currentFormat.setObsoletes(new ArrayList<>());
                            currentFormat.setFiles(new ArrayList<>());
                            break;
                        case "entry":
                            currentEntry = new Entry();
                            currentEntry.setName(getAttribute(startElement, "name"));
                            currentEntry.setFlags(getAttribute(startElement, "flags"));
                            currentEntry.setEpoch(getAttribute(startElement, "epoch"));
                            currentEntry.setVer(getAttribute(startElement, "ver"));
                            currentEntry.setRel(getAttribute(startElement, "rel"));
                            break;

                        case "file":
                            currentFile = new PackageFile();
                            currentFile.setType(getAttribute(startElement, "type"));
                            currentElement = "file";
                            break;

                        case "header-range":
                            currentHeaderRange = new HeaderRange();
                            currentHeaderRange.setStart(Long.parseLong(getAttribute(startElement, "start")));
                            currentHeaderRange.setEnd(Long.parseLong(getAttribute(startElement, "end")));
                            currentFormat.setHeaderRange(currentHeaderRange);
                            currentElement = "header-range";
                            break;

                        case "requires":
                            currentParent = "requires";
                            break;

                        case "provides":
                            currentParent = "provides";
                            break;

                        case "conflicts":
                            currentParent = "conflicts";
                            break;
                        case "obsoletes":
                            currentParent = "obsoletes";
                            break;
                        case "license":
                            currentElement = "license";
                            break;

                        case "vendor":
                            currentElement = "vendor";
                            break;
                        case "group":
                            currentElement = "group";
                            break;
                        case "buildhost":
                            currentElement = "buildhost";
                            break;
                        case "sourcerpm":
                            currentElement = "sourcerpm";
                            break;
                    }

                } else if (event.isCharacters()) {
                    Characters characters = event.asCharacters();
                    String data = characters.getData().trim();
                    if (data.isEmpty()) continue;

                    if (currentElement != null) {
                        switch (currentElement) {
                            case "name":
                                currentPackage.setName(data);
                                break;
                            case "arch":
                                currentPackage.setArch(data);
                                break;
                            case "checksum":
                                currentPackage.getChecksum().setValue(data);
                                break;
                            case "summary":
                                currentPackage.setSummary(data);
                                break;
                            case "description":
                                currentPackage.setDescription(data);
                                break;
                            case "packager":
                                currentPackage.setPackager(data);
                                break;
                            case "license":
                                currentFormat.setLicense(data);
                                break;
                            case "vendor":
                                currentFormat.setVendor(data);
                                break;
                            case "group":
                                currentFormat.setGroup(data);
                                break;
                            case "buildhost":
                                currentFormat.setBuildHost(data);
                                break;
                            case "sourcerpm":
                                currentFormat.setSourcerpm(data);
                                break;
                            case "url":
                                currentPackage.setUrl(data);
                                break;
                            case "file":
                                if (currentFile != null) {
                                    currentFile.setPath(data);
                                    currentFormat.getFiles().add(currentFile);
                                    currentFile = null;
                                }
                                break;
                        }
                        currentElement = null;
                    }

                } else if (event.isEndElement()) {
                    EndElement endElement = event.asEndElement();
                    String elementName = endElement.getName().getLocalPart();

                    switch (elementName) {
                        case "package":
                            metadata.getPackageList().add(currentPackage);
                            currentPackage = null;
                            currentFormat = null;
                            break;

                        case "entry":
                            if (currentEntry != null && currentParent != null) {
                                switch (currentParent) {
                                    case "requires":
                                        currentFormat.getRequires().add(currentEntry);
                                        break;
                                    case "provides":
                                        currentFormat.getProvides().add(currentEntry);
                                        break;
                                    case "conflicts":
                                        currentFormat.getConflicts().add(currentEntry);
                                        break;
                                    case "obsoletes":
                                        currentFormat.getObsoletes().add(currentEntry);
                                        break;

                                }
                                currentEntry = null;
                            }
                            break;

                        case "requires":
                        case "provides":
                        case "conflicts":
                            currentParent = null;
                            break;
                    }
                }
            }
            reader.close();
            return metadata;
        }
    }

    private String getAttribute(StartElement element, String name) {
        Attribute attr = element.getAttributeByName(new QName(name));
        return attr != null ? attr.getValue() : null;
    }
}
