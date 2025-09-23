package com.folib.nuget.utils.jaxb;

import com.folib.nuget.indexer.model.NuSpecPackage;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Unmarshaller;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.util.StreamReaderDelegate;
import java.io.InputStream;



@Slf4j
public class JaxbFeedParser {
    private static JAXBContext jaxbContext;
    private static XMLInputFactory xmlFactory;
    private static DocumentBuilder documentBuilder;

    @NonNull
    public static NuSpecPackage extractNuspecFromNuspecStream(InputStream stream) {
        try {
            if (jaxbContext == null) {
                jaxbContext = JAXBContext.newInstance(NuSpecPackage.class);
            }

            if (xmlFactory == null) {
                xmlFactory = XMLInputFactory.newInstance();
            }

            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            XMLStreamReader xsr = xmlFactory.createXMLStreamReader(stream);
            XMLReaderWithoutNamespace xr = new XMLReaderWithoutNamespace(xsr);

            NuSpecPackage nuSpecPackage = (NuSpecPackage)unmarshaller.unmarshal(xr);
//            String packageNameSpace = getXmlns(new BufferedInputStream(stream));
//            nuSpecPackage.setXmlns(packageNameSpace);
            return nuSpecPackage;
        } catch (Exception e) {
            log.error("Failed to parse NuSpec stream", e);
            return new NuSpecPackage();
        }
    }


    private static class XMLReaderWithoutNamespace extends StreamReaderDelegate {
        public XMLReaderWithoutNamespace(XMLStreamReader reader) {
            super(reader);
        }

        public String getAttributeNamespace(int arg0) {
            return "";
        }

        public String getNamespaceURI() {
            return "";
        }
    }

//    private static String getXmlns(InputStream stream) throws Exception {
//        if (documentBuilder == null) {
//            DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
//            documentBuilder = documentBuilderFactory.newDocumentBuilder();
//        }
//
//        Document document = documentBuilder.parse(stream);
//        return document.getDocumentElement().getAttribute("xmlns");
//    }
}
