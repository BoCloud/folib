package com.veadan.folib.metadata.indexer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StopWatch;
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
import java.io.File;
import java.io.FileInputStream;
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
    public void mergeFileListsXmlFiles(List<Path> xmlFilePaths, String savePath) throws Exception {
        for (Path filePath : xmlFilePaths) {
            aggregate(filePath);
        }
        writeToFile(Paths.get(savePath, "filelists.xml"));
    }
}
