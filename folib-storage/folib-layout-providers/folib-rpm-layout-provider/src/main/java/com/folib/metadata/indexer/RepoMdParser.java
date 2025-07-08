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
