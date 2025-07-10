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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import java.io.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class JsonToXmlConverter {

    private static final Logger logger = LoggerFactory.getLogger(JsonToXmlConverter.class);

    public void jsonToXml(List<Package> packageList, String outputFilePath) throws Exception {
        if (packageList == null || packageList.isEmpty()) {
            throw new IllegalArgumentException("Package list cannot be null or empty");
        }
        if (outputFilePath == null || outputFilePath.isBlank()) {
            throw new IllegalArgumentException("Output file path cannot be blank");
        }

        Path outputPath = Path.of(outputFilePath);
        if (Files.exists(outputPath)) {
            throw new IOException("Output file already exists and overwrite is not allowed");
        }
        logger.info("jsonToXml outputFilePath:{}",outputFilePath);
        // 指定临时文件的父目录
        // 创建临时目录并指定合法的前缀
        Path tempDir = Files.createTempDirectory("jsonToXmlTempDir");
        Path tempPath = Files.createTempFile(tempDir, "jsonToXml", ".xml");

        try (OutputStream outputStream = Files.newOutputStream(tempPath)) {
            XMLOutputFactory factory = XMLOutputFactory.newInstance();
            XMLStreamWriter writer = factory.createXMLStreamWriter(outputStream, "UTF-8");

            // 开始写入 XML 文档
            try {
                writer.writeStartDocument("UTF-8", "1.0");
                writer.writeStartElement("metadata");
                writer.writeAttribute("xmlns", "http://linux.duke.edu/metadata/common");
                writer.writeAttribute("xmlns:rpm", "http://linux.duke.edu/metadata/rpm");
                writer.writeAttribute("packages", Integer.toString(packageList.size()));

                for (Package pkg : packageList) {
                    if (pkg == null) {
                        logger.warn("Null package found in the list, skipping...");
                        continue;
                    }
                    writePackageElement(writer, pkg);
                }

                writer.writeEndElement(); // 关闭根元素 metadata
                writer.writeEndDocument();
            } finally {
                writer.close();
            }

            // 使用Transformer进行格式化
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");

            // 将临时文件内容格式化并写入最终输出文件
            try (InputStream inputStream = Files.newInputStream(tempPath);
                 OutputStream finalOutputStream = Files.newOutputStream(outputPath)) {
                StreamSource source = new StreamSource(inputStream);
                StreamResult result = new StreamResult(finalOutputStream);
                transformer.transform(source, result);
            }
        } catch (IOException | XMLStreamException e) {
            logger.error("Error writing XML: {}", e.getMessage(), e);
            throw new RuntimeException("Error writing XML", e);
        } finally {
            // 确保临时文件被删除
            if (tempPath != null && Files.exists(tempPath)) {
                Files.delete(tempPath);
            }
        }
    }

    private void writePackageElement(XMLStreamWriter writer, Package pkg) throws XMLStreamException {
        writer.writeStartElement("package");

        // 写入直接属性
        writer.writeAttribute("type", pkg.getType());
        writeSimpleElement(writer, pkg.getName(), "name");
        writeSimpleElement(writer, pkg.getArch(), "arch");

        // 处理 version（示例保留子元素，可根据需求改为属性）
        writeVersion(writer, pkg.getVersion());
        // 处理 checksum（修正部分）
        writeChecksum(writer, pkg.getChecksum());
        // 处理 summary（示例保留子元素，可根据需求改为属性）
        writeSimpleElement(writer, pkg.getSummary(), "summary");
        // 处理 description（示例保留子元素，可根据需求改为属性）
        writeSimpleElement(writer, pkg.getDescription(), "description");
        // 处理 packager（示例保留子元素，可根据需求改为属性）
        writeSimpleElement(writer, pkg.getPackager(), "packager");
        // 处理 url（示例保留子元素，可根据需求改为属性）
        writeSimpleElement(writer, pkg.getUrl(), "url");
        // 处理 time（示例保留子元素，可根据需求改为属性）
        writeTime(writer, pkg.getTime());
        // 处理 size（示例保留子元素，可根据需求改为属性）
        writeSize(writer, pkg.getSize());
        // 处理 location（示例保留子元素，可根据需求改为属性）
        writeLocation(writer, pkg.getLocation());
        // 处理 format 复杂对象
        writeFormatElement(writer, pkg.getFormat());
        // 关闭 package 元素
        writer.writeEndElement();
    }

    //通用方法：写入简单元素（标签 + 文本内容）
    private void writeSimpleElement(XMLStreamWriter writer, String content, String element) throws XMLStreamException {
        if (content != null) {
            writer.writeStartElement(element);
            writer.writeCharacters(content);
            writer.writeEndElement();
        }
    }

    // 专门处理 checksum 字段
    private void writeChecksum(XMLStreamWriter writer, Checksum checksum) throws XMLStreamException {
        if (checksum != null) {
            writer.writeStartElement("checksum");
            if (checksum.getValue() != null) {
                writer.writeAttribute("type", checksum.getType());
            }
            if (checksum.getPkgid() != null) {
                writer.writeAttribute("pkgid", checksum.getPkgid());
            }
            if (checksum.getValue() != null) {
                writer.writeCharacters(checksum.getValue());
            }
            writer.writeEndElement();
        }
    }

    private void writeVersion(XMLStreamWriter writer, Version version) throws XMLStreamException {
        if (version != null) {
            writer.writeStartElement("version");
            if (version.getEpoch() != null) {
                writer.writeAttribute("epoch", version.getEpoch());
            }
            if (version.getVer() != null) {
                writer.writeAttribute("ver", version.getVer());
            }
            if (version.getRel() != null) {
                writer.writeAttribute("rel", version.getRel());
            }
            writer.writeEndElement();
        }
    }

    private void writeTime(XMLStreamWriter writer, Time time) throws XMLStreamException {
        if (time != null) {
            writer.writeStartElement("time");
            writer.writeAttribute("file", Long.toString(time.getFile()));
            writer.writeAttribute("build", Long.toString(time.getBuild()));
            writer.writeEndElement();
        }
    }

    private void writeSize(XMLStreamWriter writer, Size size) throws XMLStreamException {
        if (size != null) {
            writer.writeStartElement("size");
            writer.writeAttribute("package", Long.toString(size.getPackageSize()));
            writer.writeAttribute("installed", Long.toString(size.getInstalled()));
            writer.writeAttribute("archive", Long.toString(size.getArchive()));
            writer.writeEndElement();
        }
    }

    private void writeLocation(XMLStreamWriter writer, Location location) throws XMLStreamException {
        if (location != null) {
            writer.writeStartElement("location");
            if (location.getHref() != null) {
                writer.writeAttribute("href", location.getHref());
            }
            writer.writeEndElement();
        }
    }

    private void writeFormatElement(XMLStreamWriter writer, Format format) throws XMLStreamException {
        if (format != null) {
            writer.writeStartElement("format");

            // 写入简单子元素（带 rpm 前缀）
            writeRpmElement(writer, "license", format.getLicense());
            writeRpmElement(writer, "vendor", format.getVendor());
            writeRpmElement(writer, "group", format.getGroup());
            writeRpmElement(writer, "buildhost", format.getBuildHost());
            writeRpmElement(writer, "sourcerpm", format.getSourcerpm());

            // 处理 header-range（空元素 + 属性）
            HeaderRange headerRange = format.getHeaderRange();
            if (headerRange != null) {
                writer.writeEmptyElement("rpm:header-range");
                writer.writeAttribute("start", Long.toString(headerRange.getStart()));
                writer.writeAttribute("end", Long.toString(headerRange.getEnd()));
            }

            writeEntryElement(writer, format.getProvides(), "rpm:provides");
            writeEntryElement(writer, format.getRequires(), "rpm:requires");
            writeEntryElement(writer, format.getConflicts(), "rpm:conflicts");
            writeEntryElement(writer, format.getObsoletes(), "rpm:obsoletes");
            writeFile(writer, format.getFiles());
            writer.writeEndElement(); // 关闭 format
        }
    }


    private void writeEntryElement(XMLStreamWriter writer, List<Entry> entryList, String key) throws XMLStreamException {
        if (entryList != null && !entryList.isEmpty()) {
            writer.writeStartElement(key);
            for (Entry entry : entryList) {
                writer.writeEmptyElement("rpm:entry");
                if (entry.getName() != null) writer.writeAttribute("name", entry.getName());
                if (entry.getFlags() != null) writer.writeAttribute("flags", entry.getFlags());
                if (entry.getEpoch() != null) writer.writeAttribute("epoch", entry.getEpoch());
                if (entry.getVer() != null) writer.writeAttribute("ver", entry.getVer());
                if (entry.getRel() != null) writer.writeAttribute("rel", entry.getRel());
            }
            writer.writeEndElement();
        }
    }

    private void writeFile(XMLStreamWriter writer, List<PackageFile> fileList) throws XMLStreamException {
        if (fileList != null) {
            for (PackageFile file : fileList) {
                writer.writeStartElement("file");
                if (file.getType() != null) {
                    writer.writeAttribute("type", file.getType());
                }
                if (file.getPath() != null) {
                    writer.writeCharacters(file.getPath());
                }
                writer.writeEndElement();
            }
        }
    }

    // 辅助方法：写入带 rpm 前缀的简单元素
    private void writeRpmElement(XMLStreamWriter writer, String localName, Object value) throws XMLStreamException {
        if (value != null) {
            writer.writeStartElement("rpm", localName, "http://linux.duke.edu/metadata/rpm");
            writer.writeCharacters(value.toString());
            writer.writeEndElement();
        }
    }
}


