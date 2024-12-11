package com.veadan.folib.metadata.indexer;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;
import com.veadan.folib.metadata.extractor.RpmFormatInterpreter;
import com.veadan.folib.metadata.extractor.RpmFormatReader;
import com.veadan.folib.metadata.extractor.RpmMetadata;
import com.veadan.folib.metadata.extractor.RpmMetadataExtractor;
import com.veadan.folib.metadata.model.Entry;
import com.veadan.folib.metadata.model.RepomdMetadata;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.io.FileUtils;
import org.redline_rpm.Dependency;
import org.redline_rpm.changelog.ChangelogEntry;
import org.redline_rpm.header.Format;
import org.redline_rpm.header.Header;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import reactor.core.publisher.Flux;
import reactor.core.scheduler.Schedulers;

import javax.inject.Inject;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.GZIPOutputStream;

public class RpmRepoIndexer {

    private static final Logger logger = LoggerFactory.getLogger(RpmRepoIndexer.class);

    private final String primaryXml;
    private final String otherXml;
    private final String repomdXml;
    private final String primaryXmlGz;
    private final String otherXmlGz;

    protected RepositoryPathResolver repositoryPathResolver;

    protected ArtifactManagementService artifactManagementService;

    private final String tempPath;

    public RpmRepoIndexer(RepositoryPathResolver repositoryPathResolver, ArtifactManagementService artifactManagementService, String tempPath) {
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.tempPath = tempPath;
        this.primaryXml = "primary.xml";
        this.otherXml = "other.xml";
        this.repomdXml = "repomd.xml";
        this.primaryXmlGz = "primary.xml.gz";
        this.otherXmlGz = "other.xml.gz";
    }

    public void indexWriter(Repository repository) throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        indexWriter( storageId,  repositoryId);
    }
    public void indexWriter(final String storageId, final String repositoryId) throws Exception {

        RootRepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
        String temp = String.join("/", tempPath, UUID.randomUUID().toString());
        Files.createDirectories(Path.of(temp));
        List<Path> paths = listPaths(repositoryPath);
        if(CollectionUtil.isEmpty(paths)){
            if(Files.isDirectory(Path.of(String.join("/", repositoryPath.getTarget().toString(),"repodata")))){
                FileUtils.deleteDirectory(new File(String.join("/", repositoryPath.getTarget().toString(),"repodata")));
            }
            return;
        }
        generateXml(paths, temp);
        //generatePrimaryXml(paths, temp);
        //generateOtherXml(paths, temp);

        Path primaryXmlPath = Path.of(String.join("/", temp, primaryXml));
        Path otherXmlPath = Path.of(String.join("/", temp, otherXml));
        if (!isFile(primaryXmlPath) || !isFile(otherXmlPath)) {
            throw new RuntimeException("primary.xml or other.xml is null");
        }

        String primaryXmlOpenSha = getSHA1(primaryXmlPath);
        String otherXmlOpenSha = getSHA1(otherXmlPath);
        long primaryOpenSize = Files.size(primaryXmlPath);

        long primaryTimestamp = Files.getLastModifiedTime(primaryXmlPath).toMillis();
        long otherTimestamp = Files.getLastModifiedTime(otherXmlPath).toMillis();

        //Path primaryXmlGzPath = Path.of(String.join("/", temp, String.join("-", primaryXmlOpenSha, primaryXmlGz)));
        //Path otherXmlGzPath = Path.of(String.join("/", temp, String.join("-", otherXmlOpenSha, otherXmlGz)));

        Path primaryXmlGzPath = Path.of(String.join("/", temp,  primaryXmlGz));
        Path otherXmlGzPath = Path.of(String.join("/", temp,  otherXmlGz));


        compressXMLToFile(primaryXmlPath, primaryXmlGzPath);
        compressXMLToFile(otherXmlPath, otherXmlGzPath);

        if (!isFile(primaryXmlGzPath) || !isFile(otherXmlGzPath)) {
            throw new RuntimeException("primary.xml.gz or other.xml.gz is null");
        }
        String primaryXmlSha = getSHA1(primaryXmlGzPath);
        String otherXmlSha = getSHA1(otherXmlGzPath);
        long primarySize = Files.size(primaryXmlGzPath);
        long otherOpenSize = Files.size(otherXmlPath);

        RepomdMetadata repomdMetadata = new RepomdMetadata();
        RepomdMetadata.XmlData primary = new RepomdMetadata.XmlData();
        primary.setHref(String.join("/",  "repodata",  String.join("-", primaryXmlSha, primaryXmlGz)));
        primary.setSize(primarySize);
        primary.setChecksum(primaryXmlSha);
        primary.setTimestamp(primaryTimestamp);
        primary.setOpenChecksum(primaryXmlOpenSha);
        primary.setOpenSize(primaryOpenSize);

        long otherSize = Files.size(otherXmlGzPath);
        RepomdMetadata.XmlData other = new RepomdMetadata.XmlData();
        other.setHref(String.join("/", "repodata", String.join("-", otherXmlSha , otherXmlGz)));
        other.setSize(otherSize);
        other.setChecksum(otherXmlSha);
        other.setTimestamp(otherTimestamp);
        other.setOpenChecksum(otherXmlOpenSha);
        other.setOpenSize(otherOpenSize);


        repomdMetadata.setPrimary(primary);
        repomdMetadata.setOther(other);
        generateRepomdXml(repomdMetadata, temp);

        Path repomdXmlPath = Path.of(String.join("/", temp, repomdXml));
        if (!isFile(repomdXmlPath)) {
            throw new RuntimeException("repomd.xml is null");
        }

        //RepositoryPath primaryPath = repositoryPathResolver.resolve(repository, String.join("/", "repodata", primaryXmlGzPath.getFileName().toString()));
        //RepositoryPath otherPath = repositoryPathResolver.resolve(repository, String.join("/", "repodata", otherXmlGzPath.getFileName().toString()));

        RepositoryPath primaryPath = repositoryPathResolver.resolve(storageId,  repositoryId,String.join("/", "repodata", String.join("-", primaryXmlSha, primaryXmlGz)));
        RepositoryPath otherPath = repositoryPathResolver.resolve(storageId,  repositoryId, String.join("/", "repodata", String.join("-", otherXmlSha, otherXmlGz)));


        RepositoryPath repomdPath = repositoryPathResolver.resolve(storageId,  repositoryId, String.join("/", "repodata", repomdXml));
        artifactManagementService.validateAndStore(primaryPath, primaryXmlGzPath);
        artifactManagementService.validateAndStore(otherPath, otherXmlGzPath);
        artifactManagementService.validateAndStore(repomdPath, repomdXmlPath);
        FileUtils.deleteDirectory(new File(temp));
    }
    public  List<Path> listPaths(Path path) throws IOException {
       return Files.walk(path).filter(this::isFileExist).collect(Collectors.toList());
    }

    //校验是否有效文件
    public boolean isFileExist(Path path) {
        if (Files.isRegularFile(path) && path.toString().endsWith(".rpm")) {
            return true;
        } else {
            return false;
        }
    }

    //校验文件是否存在
    public boolean isFile(Path path) {
        if (Files.exists(path)) {
            return true;
        } else {
            return false;
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
        }
    }

    /**
     * 计算文件的 SHA-256 哈希值。
     *
     * @param filePath 文件路径
     * @return SHA-256 哈希值的十六进制字符串
     * @throws IOException              如果读取文件失败
     * @throws NoSuchAlgorithmException 如果找不到 SHA-256 算法
     */
    public static String getSHA256(Path filePath) throws IOException, NoSuchAlgorithmException {
        // 分块大小，可以根据实际情况调整
        int bufferSize = 8192;
        byte[] buffer = new byte[bufferSize];

        // 创建消息摘要对象
        MessageDigest digest = MessageDigest.getInstance("SHA-256");

        try (FileInputStream fis = new FileInputStream(filePath.toFile())) {
            int bytesRead;
            while ((bytesRead = fis.read(buffer)) != -1) {
                // 更新消息摘要
                digest.update(buffer, 0, bytesRead);
            }
        }

        // 计算最终的哈希值
        byte[] hash = digest.digest();

        // 将字节数组转换为十六进制字符串
        StringBuilder hexString = new StringBuilder();
        for (byte b : hash) {
            String hex = Integer.toHexString(0xff & b);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }

    public static String getSHA1(Path filePath) throws NoSuchAlgorithmException, IOException {
        MessageDigest digest = MessageDigest.getInstance("SHA-1");
        try (var in = Files.newInputStream(filePath)) {
            byte[] buffer = new byte[4096];
            int read;
            while ((read = in.read(buffer)) > 0) {
                digest.update(buffer, 0, read);
            }
        }
        byte[] hash = digest.digest();
        return bytesToHex(hash);
    }

    private static String bytesToHex(byte[] hash) {
        StringBuilder hexString = new StringBuilder(2 * hash.length);
        for (int i = 0; i < hash.length; i++) {
            String hex = Integer.toHexString(0xff & hash[i]);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }
    public void generateXml(List<Path> paths, String savePath){
        try {
            DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder dbBuilder = dbFactory.newDocumentBuilder();

            Document docPrimary = dbBuilder.newDocument();
            Element primaryRootElement = docPrimary.createElement("metadata");
            primaryRootElement.setAttribute("xmlns", "http://linux.duke.edu/metadata/common");
            primaryRootElement.setAttribute("xmlns:rpm", "http://linux.duke.edu/metadata/rpm");
            primaryRootElement.setAttribute("packages", Integer.toString(paths.size()));
            docPrimary.appendChild(primaryRootElement);

            Document docOther = dbBuilder.newDocument();

            Element otherRootElement = docOther.createElement("otherdata");
            otherRootElement.setAttribute("xmlns", "http://linux.duke.edu/metadata/other");
            otherRootElement.setAttribute("packages", Integer.toString(paths.size()));
            docOther.appendChild(otherRootElement);

            for (Path path : paths) {
                RpmMetadata metadata = new RpmMetadataExtractor().extract(path);
                String fileDigests =metadata.getSha1Digest();

                // 生成primary package data
                Element packagePrimaryElement = generatePrimary(metadata, docPrimary, fileDigests);
                primaryRootElement.appendChild(packagePrimaryElement);

                // 生成other package data
                Element packageOtherElement = generateOther(metadata, docOther);
                otherRootElement.appendChild(packageOtherElement);
            }

            // Save the XML
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");

            //生成primary.xml
            DOMSource sourcePrimary = new DOMSource(docPrimary);
            StreamResult resultPrimary = new StreamResult(new File(savePath, primaryXml));
            transformer.transform(sourcePrimary, resultPrimary);

            //生成other.xml
            DOMSource sourceOther = new DOMSource(docOther);
            StreamResult resultOther = new StreamResult(new File(savePath, otherXml));
            transformer.transform(sourceOther, resultOther);

        }catch (Exception e){
            logger.error("生成other.xml或primary.xml失败");
            e.printStackTrace();
        }

    }


    public Element generatePrimary(RpmMetadata metadata, Document doc, String fileDigests) {
        Element packageElement = doc.createElement("package");
        packageElement.setAttribute("type", "rpm");


        // Name
        Element nameElement = doc.createElement("name");
        nameElement.appendChild(doc.createTextNode(metadata.getName()));
        packageElement.appendChild(nameElement);

        // Version
        Element versionElement = doc.createElement("version");
        versionElement.setAttribute("ver", metadata.getVersion());
        versionElement.setAttribute("rel", metadata.getRelease());
        packageElement.appendChild(versionElement);

        // Arch
        Element archElement = doc.createElement("arch");
        archElement.appendChild(doc.createTextNode(metadata.getArchitecture()));
        packageElement.appendChild(archElement);

        // Summary
        Element summaryElement = doc.createElement("summary");
        summaryElement.appendChild(doc.createTextNode(metadata.getSummary()));
        packageElement.appendChild(summaryElement);

        // Description
        Element descriptionElement = doc.createElement("description");
        descriptionElement.appendChild(doc.createTextNode(metadata.getDescription()));
        packageElement.appendChild(descriptionElement);

        // Packager
        Element packagerElement = doc.createElement("packager");
        packagerElement.appendChild(doc.createTextNode(metadata.getPackager()));
        packageElement.appendChild(packagerElement);

        // URL
        Element urlElement = doc.createElement("url");
        urlElement.appendChild(doc.createTextNode(metadata.getUrl()));
        packageElement.appendChild(urlElement);

        // Time
        Element timeElement = doc.createElement("time");
        timeElement.setAttribute("file", Integer.toString(metadata.getBuildTime()));
        timeElement.setAttribute("build", Integer.toString(metadata.getBuildTime()));
        packageElement.appendChild(timeElement);

        // Size
        Element sizeElement = doc.createElement("size");
        sizeElement.setAttribute("package", Long.toString(metadata.getSize()));
        sizeElement.setAttribute("installed", Integer.toString(metadata.getInstalledSize()));
        sizeElement.setAttribute("archive", Integer.toString(metadata.getArchiveSize()));
        packageElement.appendChild(sizeElement);

        // Location
        Element locationElement = doc.createElement("location");
        locationElement.setAttribute("href", String.format("Packages/%s.rpm",metadata.getHref()));
        packageElement.appendChild(locationElement);

        // Checksum
        Element checksumElement = doc.createElement("checksum");
        checksumElement.setAttribute("type", "sha");
        checksumElement.setAttribute("pkgid", "YES");
        checksumElement.appendChild(doc.createTextNode(fileDigests)); // Replace with actual checksum calculation
        packageElement.appendChild(checksumElement);

        // Format
        Element formatElement = doc.createElement("format");
        packageElement.appendChild(formatElement);

        // License
        Element licenseElement = doc.createElement("rpm:license");
        licenseElement.appendChild(doc.createTextNode(metadata.getLicense()));
        formatElement.appendChild(licenseElement);

        // Vendor
        Element vendorElement = doc.createElement("rpm:vendor");
        vendorElement.appendChild(doc.createTextNode(metadata.getVendor()));
        formatElement.appendChild(vendorElement);

        // Group
        Element groupElement = doc.createElement("rpm:group");
        groupElement.appendChild(doc.createTextNode(metadata.getGroup()));
        formatElement.appendChild(groupElement);

        // Build Host
        Element buildHostElement = doc.createElement("rpm:buildhost");
        buildHostElement.appendChild(doc.createTextNode(metadata.getBuildHost()));
        formatElement.appendChild(buildHostElement);

        // Source RPM
        Element sourceRpmElement = doc.createElement("rpm:sourcerpm");
        sourceRpmElement.appendChild(doc.createTextNode(metadata.getSourceRpm()));
        formatElement.appendChild(sourceRpmElement);

        // Header Range
        Element headerRangeElement = doc.createElement("rpm:header-range");
        headerRangeElement.setAttribute("start", Integer.toString(metadata.getHeaderStart()));  // Replace with actual start position
        headerRangeElement.setAttribute("end", Integer.toString(metadata.getHeaderEnd()));   // Replace with actual end position
        formatElement.appendChild(headerRangeElement);


        // Provides
        Element providesElement = doc.createElement("rpm:provides");
        for (Entry entry : metadata.getProvide()) {
            Element providesEntryElement1 = doc.createElement("rpm:entry");
            providesEntryElement1.setAttribute("name", entry.getName());
            providesEntryElement1.setAttribute("flags", entry.getFlags());
            providesEntryElement1.setAttribute("epoch", entry.getEpoch());
            providesEntryElement1.setAttribute("ver", entry.getVersion());
            providesEntryElement1.setAttribute("rel", entry.getRelease());
            providesElement.appendChild(providesEntryElement1);
        }
        formatElement.appendChild(providesElement);
        // Requires
        Element requiresElement = doc.createElement("rpm:requires");
        for (Entry entry : metadata.getRequire()) {
            Element requiresEntryElement1 = doc.createElement("rpm:entry");
            requiresEntryElement1.setAttribute("name", entry.getName());
            requiresEntryElement1.setAttribute("flags", entry.getFlags());
            requiresEntryElement1.setAttribute("epoch", entry.getEpoch());
            requiresEntryElement1.setAttribute("ver", entry.getVersion());
            requiresEntryElement1.setAttribute("rel", entry.getRelease());
            requiresElement.appendChild(requiresEntryElement1);
        }
        formatElement.appendChild(requiresElement);
        // Conflicts
        Element conflictsElement = doc.createElement("rpm:conflicts");
        // No conflicts in this example
        for (Entry entry : metadata.getConflict()) {
            Element conflictsEntryElement1 = doc.createElement("rpm:entry");
            conflictsEntryElement1.setAttribute("name", entry.getName());
            conflictsEntryElement1.setAttribute("flags", entry.getFlags());
            conflictsEntryElement1.setAttribute("epoch", entry.getEpoch());
            conflictsEntryElement1.setAttribute("ver", entry.getVersion());
            conflictsEntryElement1.setAttribute("rel", entry.getRelease());
            conflictsElement.appendChild(conflictsEntryElement1);
        }
        formatElement.appendChild(conflictsElement);

        // Obsoletes
        Element obsoletesElement = doc.createElement("rpm:obsoletes");
        for (Entry entry : metadata.getObsolete()) {
            Element obsoletesEntryElement1 = doc.createElement("rpm:entry");
            obsoletesEntryElement1.setAttribute("name", entry.getName());
            obsoletesEntryElement1.setAttribute("flags", entry.getFlags());
            obsoletesEntryElement1.setAttribute("epoch", entry.getEpoch());
            obsoletesEntryElement1.setAttribute("ver", entry.getVersion());
            obsoletesEntryElement1.setAttribute("rel", entry.release);
            obsoletesElement.appendChild(obsoletesEntryElement1);
        }
        formatElement.appendChild(obsoletesElement); // No obsoletes in this example

        // Files

        for (com.veadan.folib.metadata.model.File file : metadata.getFiles()) {
            if(StrUtil.isNotEmpty(file.path)){
                Element filesElement = doc.createElement("file");
                filesElement.appendChild(doc.createTextNode(file.path)); // Replace with actual checksum calculation
                formatElement.appendChild(filesElement);
            }
        }

        return packageElement;
    }

    private static Element generateOther(RpmMetadata metadata, Document doc) {
        Element packageElement = doc.createElement("package");
        packageElement.setAttribute("pkgid", metadata.getSha1Digest());
        packageElement.setAttribute("name", metadata.getName());
        packageElement.setAttribute("arch", metadata.getArchitecture());

        Element versionElement = doc.createElement("version");
        versionElement.setAttribute("ver", metadata.getVersion());
        versionElement.setAttribute("rel", metadata.getRelease());
        versionElement.setAttribute("epoch", Integer.toString(metadata.getEpoch()));
        packageElement.appendChild(versionElement);
        for (ChangelogEntry changelog : metadata.getChangeLogs()) {
            if (changelog.isComplete()) {
                Element changelogElement = doc.createElement("changelog");
                changelogElement.setAttribute("author", changelog.getUserMakingChange());
                changelogElement.setAttribute("date", String.valueOf(changelog.getChangeLogTime().getTime() / 1000));
                changelogElement.appendChild(doc.createTextNode(changelog.getDescription()));
                packageElement.appendChild(changelogElement);
            }
        }
        return packageElement;
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

            Element dataElement = doc.createElement("data");
            dataElement.setAttribute("type", "primary");
            rootElement.appendChild(dataElement);

            Element locationElement = doc.createElement("location");
            locationElement.setAttribute("href", repomdMetadata.getPrimary().getHref());
            dataElement.appendChild(locationElement);

            Element checksumElement = doc.createElement("checksum");
            checksumElement.setAttribute("type", "sha");
            checksumElement.setAttribute("pkgid", "YES");
            checksumElement.appendChild(doc.createTextNode(repomdMetadata.getPrimary().getChecksum()));
            dataElement.appendChild(checksumElement);

            Element sizeElement = doc.createElement("size");
            sizeElement.appendChild(doc.createTextNode(Long.toString(repomdMetadata.getPrimary().getSize())));
            dataElement.appendChild(sizeElement);

            Element timestampElement = doc.createElement("timestamp");
            timestampElement.appendChild(doc.createTextNode(Long.toString(repomdMetadata.getPrimary().getTimestamp())));
            dataElement.appendChild(timestampElement);

            Element openChecksumElement = doc.createElement("open-checksum");
            openChecksumElement.setAttribute("type", "sha");
            openChecksumElement.setAttribute("pkgid", "YES");
            openChecksumElement.appendChild(doc.createTextNode(repomdMetadata.getPrimary().getOpenChecksum()));
            dataElement.appendChild(openChecksumElement);

            Element openSizeElement = doc.createElement("open-size");
            openSizeElement.appendChild(doc.createTextNode(Long.toString(repomdMetadata.getPrimary().getOpenSize())));
            dataElement.appendChild(openSizeElement);
            //revision
            Element revisionElement = doc.createElement("revision");
            revisionElement.appendChild(doc.createTextNode(""));
            dataElement.appendChild(revisionElement);

            // 添加 other.xml 的数据部分
            Element otherDataElement = doc.createElement("data");
            otherDataElement.setAttribute("type", "other");
            rootElement.appendChild(otherDataElement);

            Element otherLocationElement = doc.createElement("location");
            otherLocationElement.setAttribute("href", repomdMetadata.getOther().getHref());
            otherDataElement.appendChild(otherLocationElement);

            Element otherChecksumElement = doc.createElement("checksum");
            otherChecksumElement.setAttribute("type", "sha");
            otherChecksumElement.setAttribute("pkgid", "YES");
            otherChecksumElement.appendChild(doc.createTextNode(repomdMetadata.getOther().getChecksum()));
            otherDataElement.appendChild(otherChecksumElement);

            Element otherSizeElement = doc.createElement("size");
            otherSizeElement.appendChild(doc.createTextNode(Long.toString(repomdMetadata.getOther().getSize())));
            otherDataElement.appendChild(otherSizeElement);

            Element otherOpenChecksumElement = doc.createElement("open-checksum");
            otherOpenChecksumElement.setAttribute("type", "sha");
            otherOpenChecksumElement.setAttribute("pkgid", "YES");
            otherOpenChecksumElement.appendChild(doc.createTextNode(repomdMetadata.getOther().getOpenChecksum()));
            otherDataElement.appendChild(otherOpenChecksumElement);

            Element otherOpenSizeElement = doc.createElement("open-size");
            otherOpenSizeElement.appendChild(doc.createTextNode(Long.toString(repomdMetadata.getOther().getOpenSize())));
            otherDataElement.appendChild(otherOpenSizeElement);

            Element otherTimestampElement = doc.createElement("timestamp");
            otherTimestampElement.appendChild(doc.createTextNode(String.valueOf(repomdMetadata.getOther().getTimestamp())));
            otherDataElement.appendChild(otherTimestampElement);

            //revision
            Element otherRevisionElement = doc.createElement("revision");
            otherRevisionElement.appendChild(doc.createTextNode(""));
            otherDataElement.appendChild(otherRevisionElement);

            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            DOMSource source = new DOMSource(doc);
            StreamResult result = new StreamResult(new File(savePath, repomdXml));
            transformer.transform(source, result);
        } catch (Exception e) {
            logger.error("生成repomd.xml失败");
            e.printStackTrace();
        }

    }
}

