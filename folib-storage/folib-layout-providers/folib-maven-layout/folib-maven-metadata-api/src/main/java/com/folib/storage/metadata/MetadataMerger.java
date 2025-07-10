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
package com.folib.storage.metadata;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.repository.metadata.*;
import org.apache.maven.project.artifact.PluginArtifact;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;

public class MetadataMerger
{

    private static final Logger logger = LoggerFactory.getLogger(MetadataMerger.class);

    public Metadata updateMetadataAtVersionLevel(Artifact artifact,
                                                 Metadata metadata)
    {
        if (metadata == null)
        {
            metadata = new Metadata();
            metadata.setGroupId(artifact.getGroupId());
            metadata.setArtifactId(artifact.getArtifactId());
            String newVersion = artifact.getVersion().substring(0, artifact.getVersion().indexOf("-") + 1).concat(
                    "SNAPSHOT");
            metadata.setVersion(newVersion);
        }
        // I generate timestamp once for all the merging
        String timestamp = MetadataHelper.getDateFormatInstance().format(Calendar.getInstance().getTime());

        // Update metadata o fill it for first time in case I have just created it
        Versioning versioning = metadata.getVersioning();
        if (versioning == null)
        {
            versioning = new Versioning();
            metadata.setVersioning(versioning);
        }

        Snapshot snapshot = versioning.getSnapshot();
        if (snapshot == null)
        {
            snapshot = new Snapshot();
            versioning.setSnapshot(snapshot);
        }
        snapshot.setBuildNumber(snapshot.getBuildNumber() + 1);
        snapshot.setTimestamp(timestamp.substring(0, 7) + "." + timestamp.substring(8));

        versioning.setLastUpdated(timestamp);
        versioning.getSnapshotVersions().clear();
        versioning.getSnapshotVersions().addAll(createNewSnapshotVersions(artifact.getVersion(), timestamp, snapshot.getBuildNumber()));

        return metadata;
    }

    public Metadata updateMetadataAtArtifactLevel(Artifact artifact,
                                                  Metadata metadata)
    {
        if (metadata == null)
        {
            metadata = new Metadata();
            metadata.setGroupId(artifact.getGroupId());
            metadata.setArtifactId(artifact.getArtifactId());
        }
        String newVersion = !ArtifactUtils.isSnapshot(artifact.getVersion()) ?
                            artifact.getVersion() :
                            artifact.getVersion().substring(0, artifact.getVersion().indexOf("-") + 1).concat("SNAPSHOT");
        Versioning versioning = metadata.getVersioning();
        if (versioning == null)
        {
            versioning = new Versioning();
            metadata.setVersioning(versioning);
        }
        versioning.setLatest(newVersion);
        if (!ArtifactUtils.isSnapshot(artifact.getVersion()))
        {
            versioning.setRelease(newVersion);
        }
        List<String> versions = versioning.getVersions();

        if (!versions.contains(newVersion))
        {
            versions.add(newVersion);
        }
        versioning.setLastUpdated(MetadataHelper.getDateFormatInstance().format(Calendar.getInstance().getTime()));
        return metadata;
    }

    /**
     * This method looks at the existing maven-metadata.xml plugins list and compares with incoming PluginArtifact.
     * If the incoming artifact plugin is not present in the existing Metadata then it tries to add the information
     * of the incoming artifact plugin else it ignores it.
     *
     * @param artifact
     * @param metadata
     * @return
     */
    public Metadata updateMetadataAtGroupLevel(PluginArtifact artifact,
                                               Metadata metadata)
    {
        if (metadata == null)
        {
            metadata = new Metadata();
        }
        List<Plugin> plugins = metadata.getPlugins();
        boolean found = false;
        for (Plugin plugin : plugins)
        {
            if (plugin.getArtifactId().equals(artifact.getArtifactId()))
            {
                found = true;
                break;
            }
        }
        if (!found)
        {
            Plugin plugin = null;
            plugin = getPluginFromArtifact(artifact);
            plugins.add(plugin);
        }
        return metadata;
    }

    /**
     * This method will read the artifact and find the plugin.xml in the artifact archive
     * It then reads plugin.xml using SAX parser and extracts information.
     *
     * @param artifact
     * @return
     */
    private Plugin getPluginFromArtifact(PluginArtifact artifact)
    {
        Plugin plugin = new Plugin();
        HashMap<String, String> pluginMap = extractAndReadPluginXmlFromArtifact(artifact);

        plugin.setName(pluginMap.get("name"));
        plugin.setPrefix(pluginMap.get("goalPrefix"));
        plugin.setArtifactId(artifact.getArtifactId());
        return plugin;
    }

    /**
     * This method iterates over the archived artifact until it finds the plugin.xml
     * and calls the method extractPluginXmlFile() if the plugin.xml
     * is found for extraction
     *
     * @param artifact
     */
    private HashMap<String, String> extractAndReadPluginXmlFromArtifact(PluginArtifact artifact)
    {

        FileInputStream fileInputStream = null;
        ZipInputStream zipInputStream;
        ZipEntry zipEntry;
        HashMap<String, String> pluginMap = null;
        try
        {
            fileInputStream = new FileInputStream(artifact.getFile().getCanonicalPath());
            zipInputStream = new ZipInputStream(fileInputStream);


            while ((zipEntry = zipInputStream.getNextEntry()) != null)
            {
                if (zipEntry.getName().endsWith("META-INF/maven/plugin.xml"))
                {
                    pluginMap = readPluginXmlFile(zipInputStream);
                    break;
                }
            }
        }
        catch (IOException e)
        {
            logger.error("Error occurred while trying to extract plugin.xml from artifact {}",
                         artifact.getArtifactId(), e);
        }

        return pluginMap;

    }


    private HashMap<String, String> readPluginXmlFile(ZipInputStream zis) {
        PluginHandler handler = new PluginHandler();
        SAXParserFactory saxParserFactory;
        SAXParser saxParser;

        try {
            saxParserFactory = SAXParserFactory.newInstance();

            // 禁用 DTD 和外部实体（通过 XMLReader 设置）
            saxParserFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            saxParserFactory.setFeature("http://xml.org/sax/features/external-general-entities", false);
            saxParserFactory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
            saxParserFactory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
            saxParserFactory.setXIncludeAware(false);
            saxParserFactory.setValidating(false);

            saxParser = saxParserFactory.newSAXParser();

            // 获取 XMLReader 实例并关闭实体解析
            XMLReader xmlReader = saxParser.getXMLReader();
            xmlReader.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            xmlReader.setFeature("http://xml.org/sax/features/external-general-entities", false);
            xmlReader.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
            xmlReader.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
            xmlReader.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            xmlReader.setFeature("http://sun.com/xml/internal/parser/dtd/fully-supported", false);

            // 使用自定义 EntityResolver 防止实体扩展
            xmlReader.setEntityResolver((publicId, systemId) -> new InputSource(new StringReader("")));

            // 开始解析
            xmlReader.setContentHandler(handler);
            xmlReader.parse(new InputSource(zis));

        } catch (Exception e) {
            logger.error("*** Error occurred while trying to parse the plugin.xml File ", e);
        }

        return handler.getPluginMap();
    }


    private Collection<SnapshotVersion> createNewSnapshotVersions(String version,
                                                                  String timestamp,
                                                                  int buildNumber)
    {
        Collection<SnapshotVersion> toReturn = new ArrayList<>();

        SnapshotVersion sv1 = new SnapshotVersion();
        SnapshotVersion sv2 = new SnapshotVersion();
        SnapshotVersion sv3 = new SnapshotVersion();

        toReturn.add(sv1);
        toReturn.add(sv2);
        toReturn.add(sv3);

        sv1.setClassifier("javadoc");
        sv1.setExtension("jar");
        sv1.setVersion(version.replace("SNAPSHOT",
                                       timestamp.substring(0, 7) + "." + timestamp.substring(8) + "-" + buildNumber));
        sv1.setUpdated(timestamp);

        sv2.setExtension("jar");
        sv2.setVersion(version.replace("SNAPSHOT",
                                       timestamp.substring(0, 7) + "." + timestamp.substring(8) + "-" + buildNumber));
        sv2.setUpdated(timestamp);

        sv3.setExtension("pom");
        sv3.setVersion(version.replace("SNAPSHOT",
                                       timestamp.substring(0, 7) + "." + timestamp.substring(8) + "-" + buildNumber));
        sv3.setUpdated(timestamp);

        return toReturn;
    }


}
