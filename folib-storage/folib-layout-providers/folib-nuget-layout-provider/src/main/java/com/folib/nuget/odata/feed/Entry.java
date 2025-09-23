package com.folib.nuget.odata.feed;

import com.google.common.collect.Lists;
import com.folib.nuget.rewrite.UrlRewrite;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nuget.utils.jaxb.AttributedTextNonNamespace;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.annotation.*;
import lombok.Data;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;


import java.io.OutputStream;
import java.io.Serializable;
import java.util.List;



@XmlRootElement(
        name = "entry"
)
@XmlAccessorType(XmlAccessType.FIELD)
@Slf4j
@Data
public class Entry implements Serializable, UrlRewrite {
    private String id;
    private AttributedTextNonNamespace title;
    private AttributedTextNonNamespace summary;
    private String updated;
    private Author author;
    @XmlElement(
            name = "link"
    )
    private List<Link> links;
    private Category category;
    private Content content;
    @XmlElement(
            name = "properties",
            namespace = "http://schemas.microsoft.com/ado/2007/08/dataservices/metadata"
    )
    private Properties properties;
    private transient boolean addXmlAttributes;

    public Entry() {
    }

    public Entry(Properties resultEntryProperties, String v2BaseUrl) {
        this.id = String.format("%s/Packages(Id='%s',Version='%s')", v2BaseUrl, resultEntryProperties.getId(), resultEntryProperties.getVersion());
        this.title = new AttributedTextNonNamespace(resultEntryProperties.getLowerCaseId());
        this.summary = new AttributedTextNonNamespace(resultEntryProperties.getSummary());
        this.updated = resultEntryProperties.getLastUpdated();
        this.author = new Author(resultEntryProperties.getAuthors());
        this.links = getLinks(resultEntryProperties.getId(), resultEntryProperties.getVersion());
        this.category = new Category();
        this.content = new Content(v2BaseUrl, resultEntryProperties.getLowerCaseId(), resultEntryProperties.getVersion());
        this.properties = resultEntryProperties;
    }

    private List<Link> getLinks(String packageId, String version) {
        String href = String.format("Packages(Id='%s',Version='%s')", packageId, version);
        return Lists.newArrayList(
                new Link("edit", "V2FeedPackage", href),
                new Link("self", "V2FeedPackage", href)
        );
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    @Getter
    public static class Category implements Serializable {
        @XmlAttribute
        private final String term = "NuGetGallery.OData.V2FeedPackage";
        @XmlAttribute
        private final String scheme = "http://schemas.microsoft.com/ado/2007/08/dataservices/scheme";

    }

    @XmlAccessorType(XmlAccessType.FIELD)
    @Getter
    public static class Content implements Serializable {
        @XmlAttribute
        private final String type = "application/zip";
        @XmlAttribute
        private String src;

        public Content() {
        }

        public Content(String v2BaseUrl, String packageId, String version) {
            this.src = NuGetUrlBuilder.packageContent(v2BaseUrl, packageId, version);
        }
    }


    public void writeXml(OutputStream outputStream) {
        try {
            JAXBContext context = JAXBContext.newInstance(Entry.class);
            Marshaller marshaller = context.createMarshaller();
            // 设置格式化输出（美化XML格式）
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
            // 设置XML编码
            marshaller.setProperty(Marshaller.JAXB_ENCODING, "UTF-8");
            // 禁用XML声明（可选，根据需求）
            // marshaller.setProperty(Marshaller.JAXB_FRAGMENT, true);

            // 执行序列化
            marshaller.marshal(this, outputStream);
        } catch (Exception e) {
            throw new RuntimeException("XML序列化失败", e);
        }
    }

    @Override
    public void rewrite(String v2BaseUrl) {
        this.id = String.format("%s/Packages(Id='%s',Version='%s')", v2BaseUrl, this.properties.getId(), this.properties.getVersion());
        this.links = getLinks(this.properties.getId(), this.properties.getVersion());
        this.content = new Content(v2BaseUrl, this.properties.getId(), this.properties.getVersion());
    }

    public void rewrite(String v2BaseUrl, String packageId) {
        String version = this.properties.getVersion();
        this.id = NuGetUrlBuilder.getNugetV2PackageIdUrl(v2BaseUrl, packageId, version);
        this.links = getLinks(packageId, this.properties.getVersion());
        this.content = new Content(v2BaseUrl, packageId, this.properties.getVersion());

    }
}
