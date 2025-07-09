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


package com.folib.storage.metadata.nuget.rss;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.XMLConstants;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.storage.metadata.nuget.NugetFormatException;
import com.folib.storage.metadata.nuget.Nuspec;
import com.folib.storage.metadata.nuget.StringListTypeAdapter;
import jakarta.xml.bind.DatatypeConverter;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Unmarshaller;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAnyElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.google.common.base.Joiner;
import com.google.common.base.Strings;

/**
 * Package properties in RSS
 *
 * @author Veadan
 */
@XmlRootElement(name = "properties", namespace = "http://schemas.microsoft.com/ado/2007/08/dataservices/metadata")
@XmlAccessorType(XmlAccessType.NONE)
public class EntryProperties
{

    public static EntryProperties parse(InputStream inputStream)
        throws JAXBException
    {
        JAXBContext context = JAXBContext.newInstance(EntryProperties.class);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        return (EntryProperties) unmarshaller.unmarshal(inputStream);
    }

    /**
     * Package ID.
     */
    private String id;

    /**
     * Package Version
     */
    private SemanticVersion version;

    /**
     * Package header
     */
    private String title;

    /**
     * URL icons
     */
    private String iconUrl;

    /**
     * License URL
     */
    private String licenseUrl;

    /**
     * Project URL
     */
    private String projectUrl;

    /**
     * URL of project sources
     */
    private String projectSourceUrl;

    /**
     * URL of package sources
     */
    private String packageSourceUrl;

    /**
     * Report URL
     */
    private String reportAbuseUrl;

    /**
     * Number of package downloads
     */
    private Integer downloadCount;

    /**
     * Number of package version downloads
     */
    private Integer versionDownloadCount;

    /**
     * The number of people who indicated the rating of the package
     */
    private Integer ratingsCount;

    /**
     * The number of people who indicated the rating version
     */
    private Integer versionRatingsCount;

    /**
     * Package rating
     */
    private Double rating;

    /**
     * Version rating
     */
    private Double versionRating;

    /**
     * License Verification Required
     */
    private Boolean requireLicenseAcceptance;

    /**
     * Project Description
     */
    private String description;

    /**
     * Release Notes
     */
    private String releaseNotes;

    /**
     * Tongue
     */
    private String language;

    /**
     * Package publication date
     */
    private Date published;

    /**
     * Package price
     */
    private Double price;

    /**
     * Package dependencies
     */
    private String dependencies;

    /**
     * Package hash
     */
    private String packageHash;

    /**
     * Package size
     */
    private Long packageSize;

    /**
     * External URl packet
     */
    private String externalPackageUri;

    /**
     * Package Category
     */
    private String categories;

    /**
     * Package rights
     */
    private String copyright;

    /**
     * Package Type
     */
    private String packageType;

    /**
     * Package tags
     */
    private List<String> tags;

    /**
     * Version is latest
     */
    private Boolean isLatestVersion;

    /**
     * General description
     */
    private String summary;

    /**
     * URL of package sources
     */
    private String docsUrl;

    /**
     * URL of package mailingList
     */
    private String mailingListUrl;

    /**
     * URL of bug tracker
     */
    private String bugTrackerUrl;

    /**
     * Creates an XML element of Microsoft DataTable format from version
     *
     * @param name
     *            the name of the XML element
     * @param nullable
     *            whether the element can be NULL
     * @param value
     *            element value
     * @return XML element
     * @throws ParserConfigurationException
     */
    private Element createMicrosoftElement(String name,
                                           boolean nullable,
                                           SemanticVersion value)
        throws ParserConfigurationException
    {
        String stringValue = value == null ? null : value.toString();
        return createMicrosoftElement(name, nullable, MicrosoftTypes.String, stringValue);
    }

    /**
     * Creates a Microsoft DataTable XML format element from a string
     *
     * @param name
     *            the name of the XML element
     * @param nullable
     *            whether the element can be NULL
     * @param value
     *            element value
     * @return XML element
     * @throws ParserConfigurationException
     */
    private Element createMicrosoftElement(String name,
                                           boolean nullable,
                                           String value)
        throws ParserConfigurationException
    {
        return createMicrosoftElement(name, nullable, MicrosoftTypes.String, value);
    }

    /**
     * Creates an XML element of the Microsoft DataTable format from an integer
     *
     * @param name
     *            the name of the XML element
     * @param nullable
     *            whether the element can be NULL
     * @param value
     *            element value
     * @return XML element
     * @throws ParserConfigurationException
     */
    private Element createMicrosoftElement(String name,
                                           boolean nullable,
                                           Integer value)
        throws ParserConfigurationException
    {
        String stringValue = value == null ? null : value.toString();
        return createMicrosoftElement(name, nullable, MicrosoftTypes.Int32, stringValue);
    }

    /**
     * Creates an XML element of Microsoft DataTable format from long
     *
     * @param name
     *            the name of the XML element
     * @param nullable
     *            whether the element can be NULL
     * @param value
     *            element value
     * @return XML element
     * @throws ParserConfigurationException
     */
    private Element createMicrosoftElement(String name,
                                           boolean nullable,
                                           Long value)
        throws ParserConfigurationException
    {
        String stringValue = value == null ? null : value.toString();
        return createMicrosoftElement(name, nullable, MicrosoftTypes.Int64, stringValue);
    }

    /**
     * Creates an XML element of the Microsoft DataTable format from a floating
     * point number
     *
     * @param name
     *            the name of the XML element
     * @param nullable
     *            whether the element can be NULL
     * @param value
     *            element value
     * @return XML element
     * @throws ParserConfigurationException
     */
    private Element createMicrosoftElement(String name,
                                           boolean nullable,
                                           Double value)
        throws ParserConfigurationException
    {
        String stringValue = value == null ? null : value.toString();
        return createMicrosoftElement(name, nullable, MicrosoftTypes.Double, stringValue);
    }

    /**
     * Creates a Microsoft DataTable XML format element from boolean
     *
     * @param name
     *            the name of the XML element
     * @param nullable
     *            whether the element can be NULL
     * @param value
     *            element value
     * @return XML element
     * @throws ParserConfigurationException
     */
    private Element createMicrosoftElement(String name,
                                           boolean nullable,
                                           Boolean value)
        throws ParserConfigurationException
    {
        String stringValue = value == null ? null : value.toString();
        return createMicrosoftElement(name, nullable, MicrosoftTypes.Boolean, stringValue);
    }

    /**
     * Creates a Microsoft DataTable XML format element from date / time
     *
     * @param name
     *            the name of the XML element
     * @param nullable
     *            whether the element can be NULL
     * @param value
     *            element value
     * @return XML element
     * @throws ParserConfigurationException
     */
    private Element createMicrosoftElement(String name,
                                           boolean nullable,
                                           Date value)
        throws ParserConfigurationException
    {
        String stringValue = null;
        if (value != null)
        {
            try
            {
                GregorianCalendar calendar = new GregorianCalendar();
                calendar.setTime(value);
                XMLGregorianCalendar xmlgc = DatatypeFactory.newInstance().newXMLGregorianCalendar(calendar);
                stringValue = xmlgc.toXMLFormat();
            }
            catch (DatatypeConfigurationException ex)
            {
                throw new ParserConfigurationException("Failed to convert date to XML");
            }
        }
        return createMicrosoftElement(name, nullable, MicrosoftTypes.DateTime, stringValue);
    }

    /**
     * Creates a Microsoft DataTable XML format element from a list of strings
     *
     * @param name
     *            the name of the XML element
     * @param nullable
     *            whether the element can be NULL
     * @param value
     *            element value
     * @return XML element
     * @throws ParserConfigurationException
     */
    private Element createMicrosoftElement(String name,
                                           boolean nullable,
                                           List<String> value)
        throws ParserConfigurationException
    {
        String stringValue = null;
        if (value != null)
        {
            try
            {
                StringListTypeAdapter adapter = new StringListTypeAdapter();
                stringValue = adapter.marshal(value);
            }
            catch (Exception e)
            {
                throw new ParserConfigurationException("Error converting string list");
            }
        }
        Element element = createMicrosoftElement(name, nullable, MicrosoftTypes.DateTime, stringValue);
        element.setAttributeNS(XMLConstants.XML_NS_URI, "space", "preserve");
        return element;
    }

    /**
     * Creates a Microsoft DataTable XML format element from a string
     *
     * @param name
     *            the name of the XML element
     * @param nullable
     *            whether the element can be NULL
     * @param type
     *            element type by Microsoft version
     * @param value
     *            element value
     * @return XML element
     * @throws ParserConfigurationException
     */
    private Element createMicrosoftElement(String name,
                                           boolean nullable,
                                           MicrosoftTypes type,
                                           String value)
        throws ParserConfigurationException
    {
        Document document = createDocument();
        Element element = document.createElementNS("http://schemas.microsoft.com/ado/2007/08/dataservices", name);
        if (nullable && Strings.isNullOrEmpty(value))
        {
            element.setAttributeNS("http://schemas.microsoft.com/ado/2007/08/dataservices/metadata", "null", "true");
        }
        if (type != MicrosoftTypes.String)
        {
            element.setAttributeNS("http://schemas.microsoft.com/ado/2007/08/dataservices/metadata", "type",
                                   type.toString());
        }
        element.setTextContent(value);
        document.appendChild(element);
        document.normalizeDocument();
        return document.getDocumentElement();
    }

    /**
     * Creates an XMl document for further use in conversion
     *
     * @return empty XML document
     * @throws ParserConfigurationException
     */
    private Document createDocument()
        throws ParserConfigurationException
    {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        DocumentBuilder docb = dbf.newDocumentBuilder();
        Document document = docb.newDocument();
        return document;
    }

    /**
     * Returns a list of properties serialized to XML.
     *
     * @return list of XML elements
     * @throws ParserConfigurationException
     */
    @XmlAnyElement
    public Element[] getProperties()
        throws ParserConfigurationException
    {
        List<Element> elements = new ArrayList<>();
        elements.add(createMicrosoftElement("Version", false, this.version));
        elements.add(createMicrosoftElement("Title", true, this.title));
        elements.add(createMicrosoftElement("IconUrl", true, this.iconUrl));
        elements.add(createMicrosoftElement("LicenseUrl", true, this.licenseUrl));
        elements.add(createMicrosoftElement("ProjectUrl", true, this.projectUrl));
        elements.add(createMicrosoftElement("ProjectSourceUrl", true, this.projectUrl));
        elements.add(createMicrosoftElement("PackageSourceUrl", true, this.packageSourceUrl));
        elements.add(createMicrosoftElement("DocsUrl", true, this.docsUrl));
        elements.add(createMicrosoftElement("MailingListUrl", true, this.mailingListUrl));
        elements.add(createMicrosoftElement("BugTrackerUrl", true, this.bugTrackerUrl));
        elements.add(createMicrosoftElement("ReportAbuseUrl", true, this.reportAbuseUrl));
        elements.add(createMicrosoftElement("DownloadCount", false, this.downloadCount));
        elements.add(createMicrosoftElement("VersionDownloadCount", false, this.versionDownloadCount));
        elements.add(createMicrosoftElement("RatingsCount", false, this.ratingsCount));
        elements.add(createMicrosoftElement("VersionRatingsCount", false, this.versionRatingsCount));
        elements.add(createMicrosoftElement("Rating", false, this.rating));
        elements.add(createMicrosoftElement("VersionRating", false, this.versionRating));
        elements.add(createMicrosoftElement("RequireLicenseAcceptance", false, this.requireLicenseAcceptance));
        elements.add(createMicrosoftElement("Description", false, this.description));
        elements.add(createMicrosoftElement("ReleaseNotes", true, this.releaseNotes));
        elements.add(createMicrosoftElement("Language", true, this.language));
        elements.add(createMicrosoftElement("Published", false, this.published));
        elements.add(createMicrosoftElement("Price", false, this.price));
        elements.add(createMicrosoftElement("Dependencies", false, this.dependencies));
        elements.add(createMicrosoftElement("PackageHash", false, this.packageHash));
        // TODO <d: PackageHashAlgorithm> SHA512 </ d: PackageHashAlgorithm>
        elements.add(createMicrosoftElement("PackageSize", false, this.packageSize));
        elements.add(createMicrosoftElement("ExternalPackageUri", true, this.externalPackageUri));
        elements.add(createMicrosoftElement("Categories", true, this.categories));
        elements.add(createMicrosoftElement("Copyright", true, this.copyright));
        elements.add(createMicrosoftElement("PackageType", true, this.packageType));
        elements.add(createMicrosoftElement("Tags", true, this.tags));
        elements.add(createMicrosoftElement("IsLatestVersion", false, this.isLatestVersion));
        elements.add(createMicrosoftElement("Summary", true, this.summary));
        return elements.toArray(new Element[elements.size()]);
    }

    /**
     * Restores class properties from a list of XML elements
     *
     * @param properties
     *            list of XML elements
     * @throws NugetFormatException
     *             illegal version value
     */
    public void setProperties(Element[] properties)
        throws NugetFormatException
    {
        Map<String, Element> hashMap = new HashMap<>();
        for (Element element : properties)
        {
            hashMap.put(element.getLocalName(), element);
        }
        StringListTypeAdapter adapter = new StringListTypeAdapter();

        this.version = SemanticVersion.parse(getTextContent(hashMap, "Version"));
        this.title = getTextContent(hashMap, "Title");
        this.iconUrl = getTextContent(hashMap, "IconUrl");
        this.licenseUrl = getTextContent(hashMap, "LicenseUrl");
        this.projectUrl = getTextContent(hashMap, "ProjectUrl");
        this.projectSourceUrl = getTextContent(hashMap, "ProjectSourceUrl");
        this.packageSourceUrl = getTextContent(hashMap, "PackageSourceUrl");
        this.docsUrl = getTextContent(hashMap, "DocsUrl");
        this.mailingListUrl = getTextContent(hashMap, "MailingListUrl");
        this.bugTrackerUrl = getTextContent(hashMap, "BugTrackerUrl");
        this.reportAbuseUrl = getTextContent(hashMap, "ReportAbuseUrl");
        this.downloadCount = getIntegerContent(hashMap.get("DownloadCount"));
        this.versionDownloadCount = getIntegerContent(hashMap.get("VersionDownloadCount"));
        this.ratingsCount = getIntegerContent(hashMap.get("RatingsCount"));
        this.versionRatingsCount = getIntegerContent(hashMap.get("VersionRatingsCount"));
        this.rating = getDoubleContent(hashMap.get("Rating"));
        this.versionRating = getDoubleContent(hashMap.get("VersionRating"));
        this.requireLicenseAcceptance = getBooleanContent(hashMap.get("RequireLicenseAcceptance"));
        this.description = getTextContent(hashMap, "Description");
        this.releaseNotes = getTextContent(hashMap, "ReleaseNotes");
        this.language = getTextContent(hashMap, "Language");
        this.published =  DatatypeConverter.parseDateTime(hashMap.get("Published").getTextContent())
                                                         .getTime();
        this.price = getDoubleContent(hashMap.get("Price"));
        this.dependencies = getTextContent(hashMap, "Dependencies");
        this.packageHash = getTextContent(hashMap, "PackageHash");
        this.packageSize = getLongContent(hashMap.get("PackageSize"));
        this.externalPackageUri = getStringContent(hashMap.get("ExternalPackageUri"));
        this.categories = getStringContent(hashMap.get("Categories"));
        this.copyright = getStringContent(hashMap.get("Copyright"));
        this.packageType = getStringContent(hashMap.get("PackageType"));
        this.tags = adapter.unmarshal(getTextContent(hashMap, "Tags"));
        this.isLatestVersion = getBooleanContent(hashMap.get("IsLatestVersion"));
        this.summary = getStringContent(hashMap.get("Summary"));
    }

    /**
     * If the value is NULL replaces it with an empty string
     *
     * @param value
     *            initial value
     * @return source value or empty string
     */
    private String nullToEmpty(String value)
    {
        return value == null ? "" : value;
    }

    /**
     * Sets properties from package specification
     *
     * @param nuspecFile
     *            package specification
     */
    public void setNuspec(Nuspec nuspecFile)
    {
        this.version = nuspecFile.getVersion();
        this.title = nullToEmpty(nuspecFile.getTitle());
        this.iconUrl = nullToEmpty(nuspecFile.getIconUrl());
        this.licenseUrl = nullToEmpty(nuspecFile.getLicenseUrl());
        this.projectUrl = nullToEmpty(nuspecFile.getProjectUrl());
        this.projectSourceUrl = nullToEmpty(nuspecFile.getProjectSourceUrl());
        this.packageSourceUrl = nullToEmpty(nuspecFile.getPackageSourceUrl());
        this.docsUrl = nullToEmpty(nuspecFile.getDocsUrl());
        this.mailingListUrl = nullToEmpty(nuspecFile.getMailingListUrl());
        this.bugTrackerUrl = nullToEmpty(nuspecFile.getBugTrackerUrl());
        this.reportAbuseUrl = "";
        this.requireLicenseAcceptance = nuspecFile.isRequireLicenseAcceptance();
        this.description = nuspecFile.getDescription();
        this.releaseNotes = "";
        this.language = "";
        this.price = Double.valueOf(0);
//        setDependenciesList(nuspecFile.getDependencies());
        this.externalPackageUri = "";
        this.categories = "";
        this.copyright = nuspecFile.getCopyright();
        this.packageType = "";
        this.tags = nuspecFile.getTags();
        this.summary = nuspecFile.getSummary();
    }

    public String getId()
    {
        return id;
    }

    public void setId(String id)
    {
        this.id = id;
    }

    public SemanticVersion getVersion()
    {
        return version;
    }

    public void setVersion(SemanticVersion version)
    {
        this.version = version;
    }

    public String getTitle()
    {
        return title;
    }

    public void setTitle(String title)
    {
        this.title = title;
    }

    public String getIconUrl()
    {
        return iconUrl;
    }

    public void setIconUrl(String iconUrl)
    {
        this.iconUrl = iconUrl;
    }

    public String getLicenseUrl()
    {
        return licenseUrl;
    }

    public void setLicenseUrl(String licenseUrl)
    {
        this.licenseUrl = licenseUrl;
    }

    public String getProjectSourceUrl()
    {
        return projectSourceUrl;
    }

    public void setProjectSourceUrl(String projectSourceUrl)
    {
        this.projectSourceUrl = projectSourceUrl;
    }

    public String getProjectUrl()
    {
        return projectUrl;
    }

    public void setProjectUrl(String projectUrl)
    {
        this.projectUrl = projectUrl;
    }

    public String getReportAbuseUrl()
    {
        return reportAbuseUrl;
    }

    public void setReportAbuseUrl(String reportAbuseUrl)
    {
        this.reportAbuseUrl = reportAbuseUrl;
    }

    public Integer getDownloadCount()
    {
        return downloadCount;
    }

    public void setDownloadCount(Integer downloadCount)
    {
        int cnt = downloadCount != null ? downloadCount : -1;
        this.downloadCount = cnt;
    }

    public Integer getVersionDownloadCount()
    {
        return versionDownloadCount;
    }

    public void setVersionDownloadCount(Integer versionDownloadCount)
    {
        int cnt = versionDownloadCount != null ? versionDownloadCount : -1;
        this.versionDownloadCount = cnt;
    }

    public Integer getRatingsCount()
    {
        return ratingsCount;
    }

    public void setRatingsCount(Integer ratingsCount)
    {
        int cnt = ratingsCount != null ? ratingsCount : 0;
        this.ratingsCount = cnt;
    }

    public Integer getVersionRatingsCount()
    {
        return versionRatingsCount;
    }

    public void setVersionRatingsCount(Integer versionRatingsCount)
    {
        int cnt = versionRatingsCount != null ? versionRatingsCount : -1;
        this.versionRatingsCount = cnt;
    }

    public Double getRating()
    {
        return rating;
    }

    public void setRating(Double rating)
    {
        double cnt = rating != null ? rating : -1;
        this.rating = cnt;
    }

    public Double getVersionRating()
    {
        return versionRating;
    }

    public void setVersionRating(Double versionRating)
    {
        double cnt = versionRating != null ? versionRating : -1;
        this.versionRating = cnt;
    }

    public Boolean getRequireLicenseAcceptance()
    {
        return requireLicenseAcceptance;
    }

    public void setRequireLicenseAcceptance(Boolean requireLicenseAcceptance)
    {
        boolean b = requireLicenseAcceptance == null ? false : requireLicenseAcceptance;
        this.requireLicenseAcceptance = b;
    }

    public String getDescription()
    {
        return description;
    }

    public void setDescription(String description)
    {
        this.description = description;
    }

    public String getReleaseNotes()
    {
        return releaseNotes;
    }

    public void setReleaseNotes(String releaseNotes)
    {
        this.releaseNotes = releaseNotes;
    }

    public String getLanguage()
    {
        return language;
    }

    public void setLanguage(String language)
    {
        this.language = language;
    }

    /**
     * @return package publication date
     */
    public Date getPublished()
    {
        return published;
    }

    /**
     * @param published
     *            package publication date
     */
    public void setPublished(Date published)
    {
        this.published = published;
    }

    public Double getPrice()
    {
        return price;
    }

    public void setPrice(Double price)
    {
        this.price = price;
    }

    public String getDependencies()
    {
        return dependencies;
    }

    public void setDependencies(String dependencies)
    {
        this.dependencies = dependencies;
    }

    public String getPackageHash()
    {
        return packageHash;
    }

    public void setPackageHash(String packageHash)
    {
        this.packageHash = packageHash;
    }

    public Long getPackageSize()
    {
        return packageSize;
    }

    public void setPackageSize(Long packageSize)
    {
        this.packageSize = packageSize;
    }

    public String getExternalPackageUri()
    {
        return externalPackageUri;
    }

    public void setExternalPackageUri(String externalPackageUri)
    {
        this.externalPackageUri = externalPackageUri;
    }

    public String getCategories()
    {
        return categories;
    }

    public void setCategories(String categories)
    {
        this.categories = categories;
    }

    public String getCopyright()
    {
        return copyright;
    }

    public void setCopyright(String copyright)
    {
        this.copyright = copyright;
    }

    public String getPackageType()
    {
        return packageType;
    }

    public void setPackageType(String packageType)
    {
        this.packageType = packageType;
    }

    public void setBugTrackerUrl(String bugTrackerUrl)
    {
        this.bugTrackerUrl = bugTrackerUrl;
    }

    public String getBugTrackerUrl()
    {
        return this.bugTrackerUrl;
    }

    public void setDocsUrl(String docsUrl)
    {
        this.docsUrl = docsUrl;
    }

    public String getDocsUrl()
    {
        return this.docsUrl;
    }

    public void setPackageSourceUrl(String packageSourceUrl)
    {
        this.packageSourceUrl = packageSourceUrl;
    }

    public String getPackageSourceUrl()
    {
        return this.packageSourceUrl;
    }

    public void setMailingListUrl(String mailingListUrl)
    {
        this.mailingListUrl = mailingListUrl;
    }

    public String getMailingListUrl()
    {
        return this.mailingListUrl;
    }

    public List<String> getTags()
    {
        if (tags == null)
        {
            tags = new ArrayList<>();
        }
        return tags;
    }

    public void setTags(List<String> tags)
    {
        this.tags = tags;
    }

    public Boolean getIsLatestVersion()
    {
        return isLatestVersion;
    }

    public void setIsLatestVersion(Boolean isLatestVersion)
    {
        boolean b = isLatestVersion == null ? false : isLatestVersion;
        this.isLatestVersion = b;
    }

    public String getSummary()
    {
        if (summary == null)
        {
            summary = "";
        }
        return summary;
    }

    public void setSummary(String summary)
    {
        this.summary = summary;
    }

    /**
     * @param dependencies
     *            list of dependencies
     */
//    public void setDependenciesList(List<Dependency> dependencies)
//    {
//        // TODO Understand Dependency Groups
//        if (dependencies == null || dependencies.isEmpty())
//        {
//            this.dependencies = "";
//        }
//        else
//        {
//            Joiner joiner = Joiner.on(",").skipNulls();
//            this.dependencies = joiner.join(dependencies);
//        }
//    }

    /**
     * @return package dependency list
     * @throws NugetFormatException
     *             version format error
     */
//    public List<Dependency> getDependenciesList()
//        throws NugetFormatException
//    {
//        ArrayList<Dependency> list = new ArrayList<>();
//        if (dependencies == null || dependencies.isEmpty())
//        {
//            return list;
//        }
//        String cleanDependencies = dependencies.replaceAll("", "");
//        for (String dependencyString : cleanDependencies.split("[\\ |]"))
//        {
//            Dependency dependency = Dependency.parseString(dependencyString);
//            if (dependency != null)
//            {
//                list.add(dependency);
//            }
//        }
//        return list;
//    }

    /**
     * Extracts the integer content of an element
     *
     * @param element
     *            XML element
     * @return integer value
     */
    private Integer getIntegerContent(Element element)
    {
        if (element == null || element.getTextContent() == null)
        {
            return null;
        }
        return Integer.decode(element.getTextContent().trim());
    }

    /**
     * Retrieves the integer value of the high precision element content.
     *
     * @param element
     *            XML element
     * @return integer value of increased accuracy
     */
    private Long getLongContent(Element element)
    {
        if (element == null || element.getTextContent() == null)
        {
            return null;
        }
        return Long.decode(element.getTextContent());
    }

    /**
     * Retrieves the value of an item's contents as a floating point number.
     *
     * @param element
     *            XML element
     * @return floating point number
     */
    private Double getDoubleContent(Element element)
    {
        if (element == null || element.getTextContent() == null)
        {
            return null;
        }
        return Double.parseDouble(element.getTextContent());
    }

    /**
     * Retrieves the value of the element's content as a boolean.
     *
     * @param element
     *            XML element
     * @return boolean
     */
    private Boolean getBooleanContent(Element element)
    {
        if (element == null || element.getTextContent() == null)
        {
            return null;
        }
        return Boolean.parseBoolean(element.getTextContent());
    }

    /**
     * Extracts the value of the contents of an element as a string
     *
     * @param element
     *            XML element
     * @return string
     */
    private String getStringContent(Element element)
    {
        if (element == null)
        {
            return null;
        }
        return element.getTextContent();
    }

    /**
     * Text content of specified element
     *
     * @param map
     *            map with elements
     * @param key
     *            element key
     * @return text or <b> null </ b>
     */
    private String getTextContent(Map<String, Element> map,
                                  String key)
    {
        Element result = map.get(key);
        if (result == null)
        {
            return "";
        }
        else
        {
            return result.getTextContent();
        }
    }
}
