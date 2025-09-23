package com.folib.nuget.odata.feed;

import com.google.common.collect.Lists;
import com.folib.nuget.rewrite.UrlRewrite;
import com.folib.utils.PathUtils;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.annotation.*;
import lombok.Data;
import lombok.NoArgsConstructor;


import java.io.OutputStream;
import java.io.Serializable;
import java.time.Instant;
import java.util.List;




@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(
        name = "feed"
)
@Data
@NoArgsConstructor
public class Feed implements Serializable, UrlRewrite {
    private static final int DEFAULT_SKIP_SIZE = 100;
    private static final String SKIP_PARAM = "$skip";
    @XmlAttribute(
            name = "xml:base"
    )
    private String xmlBaseAttribute;
    private String title;
    private String id;
    private String updated;
    @XmlElement(
            name = "link"
    )
    private List<Link> links;
    @XmlElement(
            namespace = "http://schemas.microsoft.com/ado/2007/08/dataservices/metadata"
    )
    private String count;
    @XmlElement(
            name = "entry"
    )
    private List<Entry> entries = Lists.newLinkedList();
    private Author author;

    public Feed(String baseUrl, String title) {
        this.xmlBaseAttribute = PathUtils.addTrailingSlash(baseUrl);
        this.title = title;
        this.id = "http://schemas.datacontract.org/2004/07/";
        this.updated = Instant.now().toString();
        this.links = Lists.newArrayList(new Link[]{new Link("self", title, title)});
    }

    public Feed(String baseUrl, List<Entry> entries, String title) {
        this(baseUrl, title);
        this.entries = entries;
        this.count = String.valueOf(entries.size());
    }

    public void writeXml(OutputStream outputStream) {
        try {
            JAXBContext context = JAXBContext.newInstance(Feed.class);
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
        this.links = Lists.newArrayList(new Link[]{new Link("self", title, title)});
        for (Entry entry : entries) {
            entry.rewrite(v2BaseUrl);
        }
    }

    @Override
    public void rewrite(String v2BaseUrl, String packageId) {
        this.links = Lists.newArrayList(new Link[]{new Link("self", title, title)});
        for (Entry entry : entries) {
            entry.rewrite(v2BaseUrl, packageId);
        }
    }
}
