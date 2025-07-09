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


package com.folib.storage.metadata.nuget;

import com.folib.artifact.coordinates.versioning.SemanticVersion;

import javax.xml.transform.sax.SAXSource;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import jakarta.xml.bind.*;
import jakarta.xml.bind.annotation.*;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * Class containing information about the NuGet package
 * 
 * @author Veadan
 */
@XmlRootElement(name = "package", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
@XmlAccessorType(XmlAccessType.NONE)
public class Nuspec implements Serializable
{

    /**
     * Namespace for NuGet 2012 package specification
     */
    public static final String NUSPEC_XML_NAMESPACE_2012 = "http://schemas.microsoft.com/packaging/2011/10/nuspec.xsd";

    /**
     * Namespace for NuGet 2013 package specification
     */
    public static final String NUSPEC_XML_NAMESPACE_2013 = "http://schemas.microsoft.com/packaging/2012/06/nuspec.xsd";

    /**
     * V3 namespace
     */
    public static final String NUSPEC_XML_NAMESPACE_2016 = "http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd";

    /**
     * Another .nuspec XSD schema.
     */
    public static final String NUSPEC_XML_NAMESPACE_2013_01 = "http://schemas.microsoft.com/packaging/2013/01/nuspec.xsd";

    /**
     * Namespace for NuGet 2011 package specification
     */
    public static final String NUSPEC_XML_NAMESPACE_2011 = "http://schemas.microsoft.com/packaging/2011/08/nuspec.xsd";

    /**
     * Namespace for NuGet 2010 package specification
     */
    public static final String NUSPEC_XML_NAMESPACE_2010 = "http://schemas.microsoft.com/packaging/2010/07/nuspec.xsd";

    /**
     * V7 namespace
     */
    public static final String NUSPEC_XML_NAMESPACE_2017 = "http://schemas.microsoft.com/packaging/2015/06/nuspec.xsd";

    /**
     * Empty namespace
     */
    public static final String NUSPEC_XML_NAMESPACE_EMPTY = "";

    /**
     * File extension
     */
    public static final String DEFAULT_FILE_EXTENSION = ".nuspec";

    /**
     * Package Metadata
     */
    @XmlElement(name = "metadata", namespace = NUSPEC_XML_NAMESPACE_2011)
    private Metadata metadata;

    /**
     * Files
     */
    @XmlElement(name = "file", namespace = NUSPEC_XML_NAMESPACE_2011)
    @XmlElementWrapper(name = "files", namespace = NUSPEC_XML_NAMESPACE_2011)
    private List<NugetFile> files;

    /**
     * Recovers package information from XML
     *
     * @param inputStream
     *            XML
     * @return recognized package information
     * @throws NugetFormatException
     *             XML does not conform to the NuGet specification
     */
    public static Nuspec parse(InputStream inputStream)
        throws NugetFormatException
    {
        try
        {
            JAXBContext context = JAXBContext.newInstance(Nuspec.class);
            Unmarshaller unmarshaller = context.createUnmarshaller();
            unmarshaller.setEventHandler(new NuspecXmlValidationEventHandler());
            XMLReader reader = XMLReaderFactory.createXMLReader();
            NugetNamespaceFilter inFilter = new NugetNamespaceFilter();
            inFilter.setParent(reader);
            InputSource inputSource = new InputSource(inputStream);
            SAXSource saxSource = new SAXSource(inFilter, inputSource);
            Nuspec result = (Nuspec) unmarshaller.unmarshal(saxSource);
            return result;
        }
        catch (JAXBException | SAXException e)
        {
            throw new NugetFormatException("Can not read nuspec from XML stream", e);
        }
    }

    public Metadata getMetadata()
    {
        if (metadata == null)
        {
            metadata = new Metadata();
        }
        return metadata;
    }

    /**
     * @return files
     */
    public List<NugetFile> getFiles()
    {
        if (files == null)
        {
            this.files = new ArrayList<>();
        }
        return files;
    }

    /**
     * @return Unique package identifier
     */
    public String getId()
    {
        return getMetadata().id;
    }

    /**
     * @return package version
     */
    public SemanticVersion getVersion()
    {
        return getMetadata().version;
    }

    /**
     * @return Title
     */
    public String getTitle()
    {
        return getMetadata().title;
    }

    /**
     * @return List of package authors
     */
    public String getAuthors()
    {
        return getMetadata().authors;
    }

    /**
     * @return List of package owners
     */
    public String getOwners()
    {
        return getMetadata().owners;
    }

    /**
     * @return Whether a license request is required
     */
    public boolean isRequireLicenseAcceptance()
    {
        if (getMetadata().requireLicenseAcceptance == null)
        {
            return false;
        }
        else
        {
            return getMetadata().requireLicenseAcceptance;
        }
    }

    /**
     * @return package description
     */
    public String getDescription()
    {
        return getMetadata().description;
    }

    /**
     * @return license URL
     */
    public String getLicenseUrl()
    {
        return getMetadata().licenseUrl;
    }

    /**
     * @return project URL
     */
    public String getProjectUrl()
    {
        return getMetadata().projectUrl;
    }

    /**
     * @return URL of project sources
     */
    public String getProjectSourceUrl()
    {
        return getMetadata().projectSourceUrl;
    }

    /**
     * @return URL of package documentation
     */
    public String getPackageSourceUrl()
    {
        return getMetadata().packageSourceUrl;
    }

    /**
     * @return URL of package sources
     */
    public String getDocsUrl()
    {
        return getMetadata().docsUrl;
    }

    /**
     * @return URL of package mailingList
     */
    public String getMailingListUrl()
    {
        return getMetadata().mailingListUrl;
    }

    /**
     * @return URL of bug tracker
     */
    public String getBugTrackerUrl()
    {
        return getMetadata().bugTrackerUrl;
    }

    /**
     * @return url icons
     */
    public String getIconUrl()
    {
        return getMetadata().iconUrl;
    }

    /**
     *
     * @return release notes
     */
    public String getReleaseNotes()
    {
        return this.getMetadata().releaseNotes;
    }

    /**
     *
     * @param releaseNotes
     *            release notes
     */
    public void setReleaseNotes(String releaseNotes)
    {
        this.getMetadata().releaseNotes = releaseNotes;
    }

    /**
     * @return Short Package Description
     */
    public String getSummary()
    {
        return getMetadata().summary;
    }

    /**
     * @return Who owns the rights to the package
     */
    public String getCopyright()
    {
        return getMetadata().copyright;
    }

    /**
     * @return Language
     */
    public String getLanguage()
    {
        return getMetadata().language;
    }

    /**
     * @return list of tags
     */
    public List<String> getTags()
    {
        if (getMetadata().tags == null)
        {
            return new ArrayList<>();
        }
        return getMetadata().tags;
    }

    /**
     * @return List of links
     */
    public List<Reference> getReferences()
    {
        if (getMetadata().references == null)
        {
            return new ArrayList<>();
        }
        return getMetadata().references;
    }

    /**
     * @return dependencies of packages, including those in groups
     */
//    public List<Dependency> getDependencies()
//    {
//        if (getMetadata().dependencies == null)
//        {
//            return new ArrayList<>();
//        }
//        return getMetadata().dependencies.getDependencies();
//    }

    /**
     *
     * @return dependency groups, including root
     */
//    public List<DependenciesGroup> getDependenciesGroups()
//    {
//        return getMetadata().dependencies.getGroups();
//    }

    /**
     * @return depending on the assemblies that come with the .NET package
     */
    public List<FrameworkAssembly> getFrameworkAssembly()
    {
        if (getMetadata().frameworkAssembly == null)
        {
            getMetadata().frameworkAssembly = new ArrayList<>();
        }
        return getMetadata().frameworkAssembly;
    }

    /**
     * @param frameworkAssembly
     *            depending on the assemblies that come with the .NET package
     */
    public void setFrameworkAssembly(List<FrameworkAssembly> frameworkAssembly)
    {
        getMetadata().frameworkAssembly = frameworkAssembly;
    }

    /**
     * Saves specification to data stream
     *
     * @param outputStream
     *            stream for recording
     * @throws JAXBException
     *             XML Conjugation Error
     */
    public void saveTo(OutputStream outputStream)
        throws JAXBException
    {
        JAXBContext context = JAXBContext.newInstance(this.getClass());
        Marshaller marshaller = context.createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        marshaller.marshal(this, outputStream);
    }

    /**
     * Class providing error validation in the NuSpec XML file structure
     */
    private static class NuspecXmlValidationEventHandler implements ValidationEventHandler
    {

        @Override
        public boolean handleEvent(ValidationEvent event)
        {
            return false;
        }
    }

    /**
     * Class containing meta package data NuGet
     */
    public static class Metadata implements Serializable
    {

        /**
         * Unique package identifier
         */
        @XmlElement(name = "id", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String id;

        /**
         * Package Version
         */
        @XmlElement(name = "version", namespace = NUSPEC_XML_NAMESPACE_2011)
        @XmlJavaTypeAdapter(value = VersionTypeAdapter.class)
        public SemanticVersion version;

        /**
         * Title
         */
        @XmlElement(name = "title", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String title;

        /**
         * List authors package
         */
        @XmlElement(name = "authors", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String authors;

        /**
         * List of package owners
         */
        @XmlElement(name = "owners", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String owners;

        /**
         * License URL
         */
        @XmlElement(name = "licenseUrl", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String licenseUrl;

        /**
         * Project URL
         */
        @XmlElement(name = "projectUrl", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String projectUrl;

        /**
         * URL of project sources
         */
        @XmlElement(name = "projectSourceUrl", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String projectSourceUrl;

        /**
         * URL of package sources
         */
        @XmlElement(name = "packageSourceUrl", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String packageSourceUrl;

        /**
         * URL of package documentation
         */
        @XmlElement(name = "docsUrl", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String docsUrl;

        /**
         * URL of package mailingList
         */
        @XmlElement(name = "mailingListUrl", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String mailingListUrl;

        /**
         * URL of bug tracker
         */
        @XmlElement(name = "bugTrackerUrl", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String bugTrackerUrl;

        /**
         * URL icons
         */
        @XmlElement(name = "iconUrl", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String iconUrl;

        /**
         * Dependencies on .NET assembly
         */
        @XmlElement(name = "frameworkAssembly", namespace = NUSPEC_XML_NAMESPACE_2011)
        @XmlElementWrapper(name = "frameworkAssemblies", namespace = NUSPEC_XML_NAMESPACE_2011)
        public List<FrameworkAssembly> frameworkAssembly;

        /**
         * Is a license request required?
         */
        @XmlElement(name = "requireLicenseAcceptance", namespace = NUSPEC_XML_NAMESPACE_2011)
        public Boolean requireLicenseAcceptance;

        /**
         * Package Description
         */
        @XmlElement(name = "description", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String description;

        /**
         * Release Notes
         */
        @XmlElement(name = "releaseNotes", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String releaseNotes;

        /**
         * Short description
         */
        @XmlElement(name = "summary", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String summary;

        /**
         * Who is entitled to the package?
         */
        @XmlElement(name = "copyright", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String copyright;

        /**
         * Tongue
         */
        @XmlElement(name = "language", namespace = NUSPEC_XML_NAMESPACE_2011)
        public String language;

        /**
         * List of labels, separated by commas
         */
        @XmlElement(name = "tags", namespace = NUSPEC_XML_NAMESPACE_2011)
        @XmlJavaTypeAdapter(value = StringListTypeAdapter.class)
        public List<String> tags;

        /**
         * List of links
         */
        @XmlElementWrapper(name = "references", namespace = NUSPEC_XML_NAMESPACE_2011)
        @XmlElement(name = "reference", namespace = NUSPEC_XML_NAMESPACE_2011)
        public List<Reference> references;

        /**
         * List of dependencies
         */
        @XmlElement(name = "dependencies", namespace = NUSPEC_XML_NAMESPACE_2011)
        public Dependencies dependencies;

        /**
         * Sign "Development Only Dependency"
         */
        @XmlElement(name = "developmentDependency", namespace = NUSPEC_XML_NAMESPACE_2011)
        public Boolean developmentDependency;

        /**
         * For internal use Nuget.
         */
        @XmlElement(name = "serviceable", namespace = NUSPEC_XML_NAMESPACE_2011)
        public Boolean serviceable;

        /**
         * Minimum required version of Nuget client to work with the package.
         */
        @XmlElement(name = "minClientVersion", namespace = NUSPEC_XML_NAMESPACE_2011)
        public Boolean minClientVersion;

        @XmlElementWrapper(name = "packageTypes", namespace = NUSPEC_XML_NAMESPACE_2011)
        @XmlElement(name = "packageType", namespace = NUSPEC_XML_NAMESPACE_2011)
        public List<PackageType> packageType;

        @XmlElementWrapper(name = "contentFiles", namespace = NUSPEC_XML_NAMESPACE_2011)
        @XmlElement(name = "files", namespace = NUSPEC_XML_NAMESPACE_2011)
        public List<ContentFile> contentFile;

        @XmlElement(name = "repository", namespace = NUSPEC_XML_NAMESPACE_2011)
        public Repository repository;

        @XmlElementWrapper(name = "files", namespace = NUSPEC_XML_NAMESPACE_2011)
        @XmlElement(name = "file", namespace = NUSPEC_XML_NAMESPACE_2011)
        public List<File> file;
    }

    public static class NugetFile implements Serializable
    {

        @XmlAttribute(name = "src")
        public String src;

        @XmlAttribute(name = "target")
        public String target;

        @XmlAttribute(name = "exclude")
        public String exclude;

    }
}
