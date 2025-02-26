package com.veadan.folib.metadata.indexer;

import org.apache.lucene.util.AttributeImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.parameters.P;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.stream.*;
import javax.xml.stream.events.*;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.zip.GZIPInputStream;

public class PrimaryXmlMerger {

    private static final Logger logger = LoggerFactory.getLogger(PrimaryXmlMerger.class);
    private Map<String, Package> packages = new HashMap<>();
    private StringBuilder buffer = new StringBuilder(); // 用于存储字符数据
    private static String currentLocationHref = null; // 用于存储location的href属性值


    public void aggregate(Path filePath) throws Exception {
        logger.info("PrimaryXmlMerger aggregate file: " + filePath);
        if(Files.exists(filePath)){
            logger.warn("PrimaryXmlMerger aggregate file not exists: " + filePath);
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
        logger.info("PrimaryXmlMerger writeToFile outputPath: " + outputPath);
        DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
        Document doc = docBuilder.newDocument();

        Element rootElement = doc.createElementNS("http://linux.duke.edu/metadata/common", "metadata");
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

    static class PackageHandler extends DefaultHandler {
        private Map<String, Package> packages;
        private Package currentPackage = null;
        private StringBuilder buffer = new StringBuilder();
        private String currentChecksumType = null;
        private String currentChecksumValue = null;
        private String currentVersionEpoch = null;
        private String currentVersionVer = null;
        private String currentVersionRel = null;
        private String currentTimeBuild = null;
        private String currentTimeFile = null;
        private String currentSizeArchive = null;
        private String currentSizeInstalled = null;
        private String currentSizePackage = null;

        public PackageHandler(Map<String, Package> packages) {
            this.packages = packages;
        }

        @Override
        public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
            if ("package".equals(qName)) {
                currentPackage = new Package(attributes.getValue("type"));
            } else if ("version".equals(qName)) {
                currentVersionEpoch = attributes.getValue("epoch");
                currentVersionVer = attributes.getValue("ver");
                currentVersionRel = attributes.getValue("rel");
            } else if ("checksum".equals(qName)) {
                currentChecksumType = attributes.getValue("type");
                currentChecksumValue = "";
            } else if ("time".equals(qName)) {
                currentTimeBuild = attributes.getValue("build");
                currentTimeFile = attributes.getValue("file");
            } else if ("size".equals(qName)) {
                currentSizeArchive = attributes.getValue("archive");
                currentSizeInstalled = attributes.getValue("installed");
                currentSizePackage = attributes.getValue("package");
            } else if ("location".equals(qName)) {
                currentLocationHref = attributes.getValue("href"); // 获取location的href属性值
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
                switch (qName) {
                    case "name":
                        currentPackage.setName(buffer.toString().trim());
                        break;
                    case "arch":
                        currentPackage.setArch(buffer.toString().trim());
                        break;
                    case "version":
                        currentPackage.setVersion(currentVersionEpoch, currentVersionVer, currentVersionRel);
                        break;
                    case "checksum":
                        currentPackage.setChecksum(currentChecksumType, buffer.toString().trim()); // 使用buffer中的内容
                        break;
                    case "summary":
                        currentPackage.setSummary(buffer.toString().trim());
                        break;
                    case "description":
                        currentPackage.setDescription(buffer.toString().trim());
                        break;
                    case "packager":
                        currentPackage.setPackager(buffer.toString().trim());
                        break;
                    case "url":
                        currentPackage.setUrl(buffer.toString().trim());
                        break;
                    case "time":
                        currentPackage.setTime(currentTimeBuild, currentTimeFile);
                        break;
                    case "size":
                        currentPackage.setSize(currentSizeArchive, currentSizeInstalled, currentSizePackage);
                        break;
                    case "location":
                        if (currentLocationHref != null) {
                            currentPackage.setLocation(currentLocationHref); // 使用之前保存的href属性值
                        } else {
                            currentPackage.setLocation(buffer.toString().trim()); // 如果没有href属性，则使用标签内的文本内容
                        }
                        break;
                    case "package":
                        packages.putIfAbsent(currentPackage.getChecksumValue(), currentPackage);
                        currentPackage = null;
                        break;
                }
            }
        }
    }

    static class Package {
        private String type;
        private String name;
        private String arch;
        private String epoch;
        private String version;
        private String release;
        private String checksumType;
        private String checksumValue;
        private String summary;
        private String description;
        private String packager;
        private String url;
        private String timeBuild;
        private String timeFile;
        private String sizeArchive;
        private String sizeInstalled;
        private String sizePackage;
        private String locationHref;

        public Package(String type) {
            this.type = type;
        }

        public void setName(String name) {
            this.name = name;
        }

        public void setArch(String arch) {
            this.arch = arch;
        }

        public void setVersion(String epoch, String ver, String rel) {
            this.epoch = epoch;
            this.version = ver;
            this.release = rel;
        }

        public void setChecksum(String type, String value) {
            this.checksumType = type;
            this.checksumValue = value;
        }

        public void setSummary(String summary) {
            this.summary = summary;
        }

        public void setDescription(String description) {
            this.description = description;
        }

        public void setPackager(String packager) {
            this.packager = packager;
        }

        public void setUrl(String url) {
            this.url = url;
        }

        public void setTime(String build, String file) {
            this.timeBuild = build;
            this.timeFile = file;
        }

        public void setSize(String archive, String installed, String packageSize) {
            this.sizeArchive = archive;
            this.sizeInstalled = installed;
            this.sizePackage = packageSize;
        }

        public void setLocation(String href) {
            this.locationHref = href;
        }

        public String getChecksumValue() {
            return checksumValue;
        }

        public void toXmlElement(Document doc, Element rootElement) {
            Element packageElement = doc.createElementNS("http://linux.duke.edu/metadata/common", "package");
            packageElement.setAttribute("type", type);

            createElementWithValue(doc, packageElement, "name", name);
            createElementWithValue(doc, packageElement, "arch", arch);

            Element versionElement = doc.createElementNS("http://linux.duke.edu/metadata/common", "version");
            versionElement.setAttribute("epoch", epoch);
            versionElement.setAttribute("ver", version);
            versionElement.setAttribute("rel", release);
            packageElement.appendChild(versionElement);

            Element checksumElement = doc.createElementNS("http://linux.duke.edu/metadata/common", "checksum");
            checksumElement.setAttribute("pkgid", "YES");
            checksumElement.setAttribute("type", checksumType);
            checksumElement.setTextContent(checksumValue);
            packageElement.appendChild(checksumElement);

            createElementWithValue(doc, packageElement, "summary", summary);
            createElementWithValue(doc, packageElement, "description", description);
            createElementWithValue(doc, packageElement, "packager", packager);
            createElementWithValue(doc, packageElement, "url", url);

            Element timeElement = doc.createElementNS("http://linux.duke.edu/metadata/common", "time");
            timeElement.setAttribute("build", timeBuild);
            timeElement.setAttribute("file", timeFile);
            packageElement.appendChild(timeElement);

            Element sizeElement = doc.createElementNS("http://linux.duke.edu/metadata/common", "size");
            sizeElement.setAttribute("archive", sizeArchive);
            sizeElement.setAttribute("installed", sizeInstalled);
            sizeElement.setAttribute("package", sizePackage);
            packageElement.appendChild(sizeElement);

            Element locationElement = doc.createElementNS("http://linux.duke.edu/metadata/common", "location");
            locationElement.setAttribute("href", locationHref);
            packageElement.appendChild(locationElement);

            rootElement.appendChild(packageElement);
        }

        private void createElementWithValue(Document doc, Element parent, String elementName, String value) {
            if (value != null && !value.isEmpty()) {
                Element element = doc.createElementNS("http://linux.duke.edu/metadata/common", elementName);
                element.setTextContent(value);
                parent.appendChild(element);
            }
        }
    }

    public  void mergePrimaryXmlFiles(List<Path> xmlFilePaths, String savePath) throws Exception {
        for (Path filePath : xmlFilePaths){
            aggregate(filePath);
        }
        writeToFile(Path.of(savePath, "primary.xml"));
    }


}
