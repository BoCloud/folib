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
package com.folib.metadata.indexer;


import cn.hutool.extra.spring.SpringUtil;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.event.index.IndexEventListenerRegistry;
import com.folib.event.index.IndexTypeEnum;
import com.folib.metadata.model.RepomdMetadata;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.io.RootRepositoryPath;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.repository.Repository;

import com.folib.util.SHA1Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;


import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.GZIPOutputStream;

public class RpmGroupRepoIndexer {

    private static final Logger logger = LoggerFactory.getLogger(RpmGroupRepoIndexer.class);

    private final String primaryXml;
    private final String otherXml;

    private final String primaryXmlGz;
    private final String otherXmlGz;
    private final String fileListXml;
    private final String fileListXmlGz;
    private final String tempPath;
    private final List<RepositoryPath> primaryPaths;
    private final List<RepositoryPath> otherPaths;
    private final List<RepositoryPath> fileListPaths;
    protected RepositoryPathResolver repositoryPathResolver;
    protected ArtifactManagementService artifactManagementService;
    private ConfigurationManager configurationManager;
    private static final String REPODATA_DIR = "repodata";
    private static final String REPOMD_XML = "repomd.xml";
    private static final String CHECKSUM_TYPE = "sha";

    public RpmGroupRepoIndexer(String tempPath, RepositoryPathResolver repositoryPathResolver, ArtifactManagementService artifactManagementService, ConfigurationManager configurationManager) {
        this.tempPath = String.join("/", tempPath, UUID.randomUUID().toString()).replace("//", "/");
        this.primaryXml = "primary.xml";
        this.otherXml = "other.xml";
        this.primaryXmlGz = "primary.xml.gz";
        this.otherXmlGz = "other.xml.gz";
        this.fileListXml = "filelists.xml";
        this.fileListXmlGz = "filelists.xml.gz";
        this.primaryPaths = new ArrayList<>();
        this.otherPaths = new ArrayList<>();
        this.fileListPaths = new ArrayList<>();
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.configurationManager = configurationManager;
    }

    /**
     * 聚合组合仓库指定子仓库索引
     * @param groupRepository
     * @param targetRepository
     * @throws Exception
     */
    public void aggregationIndexer(Repository groupRepository, Repository targetRepository) throws Exception {

        Set<String> storageAndRepositoryIdList = new HashSet<>();

        storageAndRepositoryIdList.add(ConfigurationUtils.getStorageIdAndRepositoryId(groupRepository.getStorage().getId(), groupRepository.getId()));
        storageAndRepositoryIdList.add(ConfigurationUtils.getStorageIdAndRepositoryId(targetRepository.getStorage().getId(), targetRepository.getId()));
        List<Path> repomdXmlPaths = new ArrayList<>();
        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            repomdXmlPaths.addAll(getRepomdXmlPath(storageAndRepositoryId));
        }
        if (repomdXmlPaths.isEmpty()) {
            logger.warn("[{}]空间下的[{}]组合仓库，子仓库没有repodata/repomd.xml文件", groupRepository.getStorage().getId(), groupRepository.getId());
            return;
        }


        for (Path path : repomdXmlPaths) {
            parserRepomdXml((RepositoryPath) path);
        }
        if (primaryPaths.isEmpty() || otherPaths.isEmpty()) {
            logger.warn("[{}]空间下的[{}]组合仓库，子仓库没有primary.xml或other.xml文件", groupRepository.getStorage().getId(), groupRepository.getId());
            return;
        }

        if (!fileListPaths.isEmpty()) {
            //合并repomd.xml
            FileListsXmlMerger fileListsXmlMerger = new FileListsXmlMerger();
            fileListsXmlMerger.mergeFileListsXmlFiles(fileListPaths, tempPath);
        }

        //合并other.xml
        OtherXmlMaerger otherXmlMaerger = new OtherXmlMaerger();
        otherXmlMaerger.mergeOtherXmlFiles(otherPaths, tempPath);

        //合并primary.xml
        PrimaryXmlMerger primaryXmlMerger = new PrimaryXmlMerger();
        primaryXmlMerger.mergePrimaryXmlFiles(primaryPaths, tempPath);

        //生成repomd.xml
        processRepository(groupRepository, tempPath);

        //发送索引更新事件
        IndexEventListenerRegistry registry = SpringUtil.getBean(IndexEventListenerRegistry.class);
        registry.dispatchUpdateIndexEvent(groupRepository.getStorage().getId(), groupRepository.getId(), IndexTypeEnum.RPM);
    }

    /**
     * 组合仓库初始化聚合索引
     * @param repository
     * @throws Exception
     */
    public void aggregationIndexer(Repository repository) throws Exception {

        //StopWatch stopWatch = new StopWatch("aggregationIndexer");
        Set<String> storageAndRepositoryIdList = new HashSet<>();

        //stopWatch.start("getGroupRepositories");
        getGroupRepositories(repository, storageAndRepositoryIdList);
        if (storageAndRepositoryIdList.isEmpty()) {
            logger.warn("[{}]空间下的[{}]组合仓库，没有子仓库", repository.getStorage().getId(), repository.getId());
            return;
        }
        //stopWatch.stop();

        //stopWatch.start("parserRepomdXml");
        List<Path> repomdXmlPaths = new ArrayList<>();
        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            repomdXmlPaths.addAll(getRepomdXmlPath(storageAndRepositoryId));
        }
        if (repomdXmlPaths.isEmpty()) {
            logger.warn("[{}]空间下的[{}]组合仓库，子仓库没有repodata/repomd.xml文件", repository.getStorage().getId(), repository.getId());
            return;
        }
        //stopWatch.stop();


        //stopWatch.start("parserRepomdXml");
        for (Path path : repomdXmlPaths) {
            parserRepomdXml((RepositoryPath) path);
        }
        if (primaryPaths.isEmpty() || otherPaths.isEmpty()) {
            logger.warn("[{}]空间下的[{}]组合仓库，子仓库没有primary.xml或other.xml文件", repository.getStorage().getId(), repository.getId());
            return;
        }
        //stopWatch.stop();


        //stopWatch.start("fileListsXmlMerger");
        if (!fileListPaths.isEmpty()) {
            //合并repomd.xml
            FileListsXmlMerger fileListsXmlMerger = new FileListsXmlMerger();
            fileListsXmlMerger.mergeFileListsXmlFiles(fileListPaths, tempPath);
            //stopWatch.stop();
        }

        //stopWatch.start("otherXmlMaerger");
        //合并other.xml
        OtherXmlMaerger otherXmlMaerger = new OtherXmlMaerger();
        otherXmlMaerger.mergeOtherXmlFiles(otherPaths, tempPath);
        //stopWatch.stop();


        //stopWatch.start("primaryXmlMaerger");
        //合并primary.xml
        PrimaryXmlMerger primaryXmlMerger = new PrimaryXmlMerger();
        primaryXmlMerger.mergePrimaryXmlFiles(primaryPaths, tempPath);
        //stopWatch.stop();


        //stopWatch.start("generateRepomdXml");
        //生成repomd.xml
        processRepository(repository, tempPath);
        //stopWatch.stop();


        //logger.info(stopWatch.prettyPrint());
    }

    public void getGroupRepositories(Repository repository, Set<String> storageAndRepositoryIdList) {

        for (String id : repository.getGroupRepositories()) {
            Repository subRepository = configurationManager.getRepository(id);
            if (subRepository == null) {
                continue;
            }
            storageAndRepositoryIdList.add(id);
            if (subRepository.getType().equals("group")) {
                getGroupRepositories(subRepository, storageAndRepositoryIdList);
            }
        }
    }

    public List<Path> getRepomdXmlPath(String storageAndRepositoryId) throws IOException {
        // 验证和清理 storageAndRepositoryId
        if (storageAndRepositoryId == null) {
            throw new IllegalArgumentException("Invalid storageAndRepositoryId");
        }

        try {
            Repository subRepository = configurationManager.getRepository(storageAndRepositoryId);
            if (subRepository == null) {
                throw new IllegalStateException("Repository not found for id: " + storageAndRepositoryId);
            }

            RootRepositoryPath path = repositoryPathResolver.resolve(subRepository.getStorage().getId(), subRepository.getId());
            if (path == null) {
                throw new IllegalStateException("Failed to resolve root repository path");
            }

            Pattern pattern = Pattern.compile("^(?!.*(/\\.temp/|/\\.trash/)).*repodata/repomd\\.xml$");

            return Files.find(path, Integer.MAX_VALUE, (p, attrs) ->
                    pattern.matcher(p.toString()).matches()
            ).collect(Collectors.toList());

        } catch (IOException e) {
            throw new IOException("Error accessing repository files", e);
        }
    }

    /**
     * 解析repomd.xml
     *
     * @param path
     * @throws Exception
     */
    public void parserRepomdXml(RepositoryPath path) throws Exception {
        RepoMdParser parser = new RepoMdParser();
        parser.parse(path.getTarget().toString());
        primaryPaths.add(getPath(path, parser.getHrefs().get("primary")));
        otherPaths.add(getPath(path, parser.getHrefs().get("other")));
        fileListPaths.add(getPath(path, parser.getHrefs().get("filelists")));
    }

    public RepositoryPath getPath(RepositoryPath repomdPath, String filePath) {
        String path = repomdPath.getPath().replace("repodata/repomd.xml", filePath);
        return repositoryPathResolver.resolve(repomdPath.getStorageId(), repomdPath.getRepositoryId(), path);
    }

    /**
     * 处理组合库索引
     *
     * @param repository
     * @param tempPath
     * @throws Exception
     */
    public void processRepository(Repository repository, String tempPath) throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        // 构建文件路径
        Path primaryXmlPath = Paths.get(tempPath, "primary.xml");
        Path otherXmlPath = Paths.get(tempPath, "other.xml");
        Path fileListXmlPath = Paths.get(tempPath, "filelists.xml");

        // 检查文件是否存在
        if (!isFile(primaryXmlPath) || !isFile(otherXmlPath) || !isFile(fileListXmlPath)) {
            throw new FileNotFoundException("One or more XML files are missing");
        }

        // 获取文件元数据并缓存结果
        Map<Path, FileMetadata> metadataMap = getFileMetadata(primaryXmlPath, otherXmlPath, fileListXmlPath);

        // 构建压缩文件路径
        Path primaryXmlGzPath = Paths.get(tempPath, "primary.xml.gz");
        Path otherXmlGzPath = Paths.get(tempPath, "other.xml.gz");
        Path fileListXmlGzPath = Paths.get(tempPath, "filelists.xml.gz");

        // 压缩文件
        compressXMLToFile(primaryXmlPath, primaryXmlGzPath);
        compressXMLToFile(otherXmlPath, otherXmlGzPath);
        compressXMLToFile(fileListXmlPath, fileListXmlGzPath);

        // 检查压缩文件是否存在且完整
        if (!isFile(primaryXmlGzPath) || !isFile(otherXmlGzPath) || !isFile(fileListXmlGzPath)) {
            throw new IOException("One or more compressed XML files are missing or corrupted");
        }

        // 获取压缩文件元数据并缓存结果
        Map<Path, FileMetadata> compressedMetadataMap = getFileMetadata(primaryXmlGzPath, otherXmlGzPath, fileListXmlGzPath);

        // 创建 RepomdMetadata 对象
        RepomdMetadata repomdMetadata = createRepomdMetadata(metadataMap, compressedMetadataMap);

        // 生成 repomd.xml 文件
        generateRepomdXml(repomdMetadata, tempPath);

        // 验证并存储文件
        storeArtifacts(storageId, repositoryId, tempPath, repomdMetadata);

        // 删除临时目录
        try (Stream<Path> walk = Files.walk(Paths.get(tempPath))) {
            walk.sorted(Comparator.reverseOrder())
                    .map(Path::toFile)
                    .forEach(File::delete);
        }
    }

    public boolean isFile(Path path) {
        return Files.exists(path) && Files.isRegularFile(path);
    }

    private Map<Path, FileMetadata> getFileMetadata(Path... paths) throws IOException, NoSuchAlgorithmException {
        Map<Path, FileMetadata> metadataMap = new HashMap<>();
        for (Path path : paths) {
            String sha1 = SHA1Util.getSHA1(path);
            long size = Files.size(path);
            long timestamp = Files.getLastModifiedTime(path).toMillis();
            metadataMap.put(path, new FileMetadata(sha1, size, timestamp));
        }
        return metadataMap;
    }

    private RepomdMetadata createRepomdMetadata(Map<Path, FileMetadata> originalMetadata, Map<Path, FileMetadata> compressedMetadata) {
        RepomdMetadata repomdMetadata = new RepomdMetadata();

        // 设置 primary 数据
        repomdMetadata.setPrimary(new RepomdMetadata.XmlData());
        setXmlData(repomdMetadata.getPrimary(), "primary.xml.gz", originalMetadata.get(Paths.get(tempPath, "primary.xml")), compressedMetadata.get(Paths.get(tempPath, "primary.xml.gz")));

        // 设置 other 数据
        repomdMetadata.setOther(new RepomdMetadata.XmlData());
        setXmlData(repomdMetadata.getOther(), "other.xml.gz", originalMetadata.get(Paths.get(tempPath, "other.xml")), compressedMetadata.get(Paths.get(tempPath, "other.xml.gz")));

        // 设置 filelists 数据
        repomdMetadata.setFilelists(new RepomdMetadata.XmlData());
        setXmlData(repomdMetadata.getFilelists(), "filelists.xml.gz", originalMetadata.get(Paths.get(tempPath, "filelists.xml")), compressedMetadata.get(Paths.get(tempPath, "filelists.xml.gz")));

        return repomdMetadata;
    }

    private void setXmlData(RepomdMetadata.XmlData xmlData, String fileName, FileMetadata originalMetadata, FileMetadata compressedMetadata) {
        xmlData.setHref(String.join("/", REPODATA_DIR, String.join("-", compressedMetadata.getSha1(), fileName)));
        xmlData.setSize(compressedMetadata.getSize());
        xmlData.setChecksum(compressedMetadata.getSha1());
        xmlData.setTimestamp(originalMetadata.getTimestamp());
        xmlData.setOpenChecksum(originalMetadata.getSha1());
        xmlData.setOpenSize(originalMetadata.getSize());
    }

    private void storeArtifacts(String storageId, String repositoryId, String tempPath, RepomdMetadata repomdMetadata) throws Exception {
        Path repomdXmlPath = Paths.get(tempPath.toString(), REPOMD_XML);
        if (!isFile(repomdXmlPath)) {
            throw new FileNotFoundException("repomd.xml is missing");
        }

        RepositoryPath primaryPath = repositoryPathResolver.resolve(storageId, repositoryId, repomdMetadata.getPrimary().getHref());
        RepositoryPath otherPath = repositoryPathResolver.resolve(storageId, repositoryId, repomdMetadata.getOther().getHref());
        RepositoryPath fileListPath = repositoryPathResolver.resolve(storageId, repositoryId, repomdMetadata.getFilelists().getHref());
        RepositoryPath repomdPath = repositoryPathResolver.resolve(storageId, repositoryId, String.join("/", REPODATA_DIR, REPOMD_XML));

        artifactManagementService.validateAndStore(primaryPath, Paths.get(tempPath, "primary.xml.gz"));
        artifactManagementService.validateAndStore(otherPath, Paths.get(tempPath, "other.xml.gz"));
        artifactManagementService.validateAndStore(fileListPath, Paths.get(tempPath, "filelists.xml.gz"));
        logger.info("storeArtifacts repomdPath:{}", repomdPath);
        logger.info("storeArtifacts repomdXmlPath:{}", repomdXmlPath);
        artifactManagementService.validateAndStore(repomdPath, repomdXmlPath);
    }

    private static class FileMetadata {
        private final String sha1;
        private final long size;
        private final long timestamp;

        public FileMetadata(String sha1, long size, long timestamp) {
            this.sha1 = sha1;
            this.size = size;
            this.timestamp = timestamp;
        }

        public String getSha1() {
            return sha1;
        }

        public long getSize() {
            return size;
        }

        public long getTimestamp() {
            return timestamp;
        }
    }


    /**
     * 压缩 XML 文件到 .gz 包。
     *
     * @param inputPath  输入文件路径
     * @param outputPath 输出文件路径
     * @throws IOException 如果读写文件时发生错误
     */
    public void compressXMLToFile(Path inputPath, Path outputPath) throws IOException {
        // 定义缓冲区大小
        int bufferSize = 8192;
        byte[] buffer = new byte[bufferSize];

        // 验证输入和输出路径的有效性
        if (!Files.exists(inputPath)) {
            throw new FileNotFoundException("Input file does not exist: " + inputPath);
        }
        if (Files.isDirectory(inputPath)) {
            throw new IllegalArgumentException("Input path is a directory: " + inputPath);
        }
        if (outputPath != null && Files.exists(outputPath) && Files.isDirectory(outputPath)) {
            throw new IllegalArgumentException("Output path is a directory: " + outputPath);
        }

        // 使用 try-with-resources 自动管理流的关闭
        try (InputStream inputStream = Files.newInputStream(inputPath);
             OutputStream outputStream = Files.newOutputStream(outputPath);
             GZIPOutputStream gzipOutputStream = new GZIPOutputStream(outputStream);
             BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(gzipOutputStream)) {

            int bytesRead;
            // 循环读取输入流直到没有更多的数据
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                // 写入缓冲区中的数据到压缩输出流
                bufferedOutputStream.write(buffer, 0, bytesRead);
            }
        } catch (IOException e) {
            // 记录异常信息
            System.err.println("An error occurred while compressing the XML file: " + e.getMessage());
            throw e; // 重新抛出异常以便上层处理
        }
    }

    private void generateRepomdXml(RepomdMetadata repomdMetadata, String savePath) throws Exception {
        try {
            DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
            Document doc = dBuilder.newDocument();

            Element rootElement = doc.createElement("repomd");
            rootElement.setAttribute("xmlns", "http://linux.duke.edu/metadata/repo");
            rootElement.setAttribute("xmlns:rpm", "http://linux.duke.edu/metadata/rpm");
            doc.appendChild(rootElement);

            // 添加 primary, other, filelists 数据部分
            for (String type : List.of("primary", "other", "filelists")) {
                Element dataElement = repomdXmlSupplementary(repomdMetadata, doc, type);
                if (dataElement != null) {
                    rootElement.appendChild(dataElement);
                }
            }

            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            DOMSource source = new DOMSource(doc);
            StreamResult result = new StreamResult(Files.newOutputStream(Path.of(savePath, REPOMD_XML)));
            transformer.transform(source, result);
        } catch (Exception e) {
            logger.error("生成repomd.xml失败: {}", e.getMessage(), e);
            throw e; // 重新抛出异常以便调用方处理
        }
    }

    public Element repomdXmlSupplementary(RepomdMetadata repomdMetadata, Document doc, String type) {
        if (repomdMetadata == null || !List.of("primary", "other", "filelists").contains(type)) {
            return null;
        }

        RepomdMetadata.XmlData dataInfo = null;
        switch (type) {
            case "primary":
                dataInfo = repomdMetadata.getPrimary();
                break;
            case "other":
                dataInfo = repomdMetadata.getOther();
                break;
            case "filelists":
                dataInfo = repomdMetadata.getFilelists();
                break;
            default:
                dataInfo = null;
                break;
        }

        if (dataInfo == null) {
            return null;
        }

        Element dataElement = doc.createElement("data");
        dataElement.setAttribute("type", type);

        Element locationElement = doc.createElement("location");
        locationElement.setAttribute("href", dataInfo.getHref());
        dataElement.appendChild(locationElement);

        Element checksumElement = doc.createElement("checksum");
        checksumElement.setAttribute("type", CHECKSUM_TYPE);
        checksumElement.setAttribute("pkgid", "YES");
        checksumElement.appendChild(doc.createTextNode(dataInfo.getChecksum()));
        dataElement.appendChild(checksumElement);

        Element sizeElement = doc.createElement("size");
        sizeElement.appendChild(doc.createTextNode(Long.toString(dataInfo.getSize())));
        dataElement.appendChild(sizeElement);

        Element timestampElement = doc.createElement("timestamp");
        timestampElement.appendChild(doc.createTextNode(Long.toString(dataInfo.getTimestamp())));
        dataElement.appendChild(timestampElement);

        Element openChecksumElement = doc.createElement("open-checksum");
        openChecksumElement.setAttribute("type", CHECKSUM_TYPE);
        openChecksumElement.setAttribute("pkgid", "YES");
        openChecksumElement.appendChild(doc.createTextNode(dataInfo.getOpenChecksum()));
        dataElement.appendChild(openChecksumElement);

        Element openSizeElement = doc.createElement("open-size");
        openSizeElement.appendChild(doc.createTextNode(Long.toString(dataInfo.getOpenSize())));
        dataElement.appendChild(openSizeElement);

        // revision
        Element revisionElement = doc.createElement("revision");
        revisionElement.appendChild(doc.createTextNode(""));
        dataElement.appendChild(revisionElement);

        return dataElement;
    }


}


