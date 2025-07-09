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
package com.folib.artifact.coordinates;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSONObject;
import com.github.zafarkhaja.semver.Version;
import com.folib.artifact.ArtifactNotFoundException;
import com.folib.providers.io.AbsFallbackRemoteArtifactInputStream;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.configuration.GoRepositoryConfigurationData;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.errors.AmbiguousObjectException;
import org.eclipse.jgit.lib.*;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevTree;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.transport.CredentialsProvider;
import org.eclipse.jgit.transport.RefSpec;
import org.eclipse.jgit.transport.TransportHttp;
import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider;
import org.eclipse.jgit.transport.http.HttpConnection;
import org.eclipse.jgit.transport.http.JDKHttpConnectionFactory;
import org.eclipse.jgit.treewalk.TreeWalk;
import org.eclipse.jgit.treewalk.filter.PathFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;

import java.io.*;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.URI;
import java.net.URL;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author veadan
 * @date 1/15/2024 9:45
 */

public class GoFallbackRemoteArtifactInputStream extends AbsFallbackRemoteArtifactInputStream {
    private static final Logger logger = LoggerFactory.getLogger(GoFallbackRemoteArtifactInputStream.class);
    private String moduleName;
    private String fileType;
    private String moduleVersion;
    private ModuleVersionType moduleVersionType;
    private String gitUrl;
    private Git git;
    private Map<Version, Ref> semverRefMap;
    private Map<String, Ref> branchRefMap;
    private final RepositoryPath repositoryPath;
    private CredentialsProvider credentialsProvider;
    private final HashMap<String, Supplier<InputStream>> fileTypeAndProcessorMap = new HashMap<>();

    // 创建一个日期格式化器，匹配 Go 中的时间格式
    private final static DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
    private final String pseudoVersionRegex = "^v\\d+\\.\\d+\\.\\d+.*\\d{14}-[a-f0-9]{12}$";
    private final Pattern pseudoVersionPattern = Pattern.compile(pseudoVersionRegex);

    public GoFallbackRemoteArtifactInputStream(RepositoryPath repositoryPath) {
        this.repositoryPath = repositoryPath;
        registerProcessor();
    }

    private void registerProcessor() {
        fileTypeAndProcessorMap.put("mod", this::mod);
        fileTypeAndProcessorMap.put("zip", this::zip);
        fileTypeAndProcessorMap.put("info", this::info);
        fileTypeAndProcessorMap.put("list", this::list);
        fileTypeAndProcessorMap.put("latest", this::latest);
    }

    @Override
    protected InputStream intiTarget() throws IOException {
        InputStream inputStream = null;
        try {
            inputStream = doInitTarget();
        } catch (Exception e) {
            URI uri = repositoryPath.toUri();
            logger.error("intiTarget fail", e);
            throw new ArtifactNotFoundException(uri, e.getMessage());
        }
        return inputStream;
    }

    private void initGitLocalRepo() throws Exception {

        String relativize = repositoryPath.relativize().toString();
        relativize = relativize.substring(0, relativize.lastIndexOf("@"));

        String cache = repositoryPath.getFileSystem().getTempPath().getTarget() + File.separator + "vcsCache" + File.separator + relativize;
        git = getGit(new File(cache));
        git.fetch()
                .setCredentialsProvider(credentialsProvider)
                .setRemote("origin")
                .setRemoveDeletedRefs(true)
                // .setRefSpecs(new RefSpec("+refs/tags/v1.0.3:refs/tags/v1.0.3"))
                //.setRefSpecs(new RefSpec(String.format("+%s:%s", tag, tag)))
                .setRefSpecs(new RefSpec("+refs/heads/*:refs/remotes/origin/*"),
                        new RefSpec("+refs/tags/*:refs/tags/*"))
                // .setDepth(1)
                .setForceUpdate(true)
                .call();
    }

    private InputStream latest() {

        try {
            //Semantic Versioning
            if (ModuleVersionType.Unknown.equals(moduleVersionType)) {
                Ref ref = git.getRepository().getRefDatabase().findRef(Constants.FETCH_HEAD);
                ObjectId commitId = ref.getObjectId();
                RevCommit revCommit = getRevCommitByCommitId(commitId);
                if (revCommit == null) {
                    throw new RuntimeException("revCommit not found,ObjectId :" + commitId);
                }
                String version = buildPseudoVer(revCommit);
                return getInfoV2(version, revCommit);
            }


            List<Version> sortedVersion = semverRefMap.keySet().stream().sorted().collect(Collectors.toList());
            if (CollectionUtil.isEmpty(sortedVersion)) {
                throw new RuntimeException("version List is empty");
            }
            Version ver = sortedVersion.get(sortedVersion.size() - 1);
            Ref ref = semverRefMap.get(ver);
            String version = ref.getName().replaceAll(Constants.R_TAGS, "");
            ObjectId commitId = ref.getObjectId();
            RevCommit revCommit = getRevCommitByCommitId(commitId);
            if (revCommit == null) {
                throw new RuntimeException("revCommit not found,ObjectId :" + commitId);
            }
            return getInfoV2(version, revCommit);

        } catch (Exception e) {
            throw new RuntimeException("process 'latest' file Exception", e);
        }
    }

    private RevCommit getRevCommitByCommitId(ObjectId commitId) throws IOException {
        RevCommit revCommit;
        try (RevWalk revWalk = new RevWalk(git.getRepository())) {
            revCommit = revWalk.parseCommit(commitId);
        }
        return revCommit;
    }

    private InputStream list() {
        //Semantic Versioning
        if (CollectionUtil.isEmpty(semverRefMap)) {
            return new ByteArrayInputStream("".getBytes());
        }
        List<String> sortedVersion = semverRefMap.keySet().stream().sorted()
                .map(version1 -> {
                    return "v" + version1.toString();
                }).collect(Collectors.toList());
        String join = String.join("\n", sortedVersion);
        return new ByteArrayInputStream(join.getBytes());
    }

    private InputStream info() {
        try {
            RevCommit commit = null;
            Ref ref = null;
            String version = null;

            switch (moduleVersionType) {
                case Branch:
                    ref = getRef();
                    if (ref == null) {
                        throw new RuntimeException("not found Ref by moduleVersion:" + moduleVersion);
                    }
                    commit = getRevCommitWithSemVer(ref);
                    version = buildPseudoVer(commit);
                    break;
                case SemVer:
                    ref = getRef();
                    if (ref == null) {
                        throw new RuntimeException("not found Ref by moduleVersion:" + moduleVersion);
                    }
                    version = ref.getName().replaceAll(Constants.R_TAGS, "");
                    commit = getRevCommitWithSemVer(ref);
                    break;
                case PseudoVer:
                    commit = getRevCommitWithPseudoVer();
                    version = buildPseudoVer(commit);
                    break;
                default:
                    throw new RuntimeException("not support VersionType:"+moduleVersionType);
            }
            if (commit == null) {
                throw new RuntimeException("RevCommit cannot be null");
            }
            if (StringUtils.isBlank(version)) {
                throw new RuntimeException("version cannot be blank");
            }
            return getInfoV2(version, commit);
        } catch (Exception e) {
            throw new RuntimeException("process '.info' file Exception", e);
        }
    }

    private String buildInfoVersion(RevCommit commit, Ref ref) {
        String version;
        if (ModuleVersionType.PseudoVer.equals(moduleVersionType)
                || ModuleVersionType.Branch.equals(moduleVersionType)
                || ModuleVersionType.Unknown.equals(moduleVersionType)) {

            version = buildPseudoVer(commit);

        } else if (ModuleVersionType.SemVer.equals(moduleVersionType)) {
            version = ref.getName().replaceAll(Constants.R_TAGS, "");
        } else {
            throw new RuntimeException("unsupported moduleVersionType :" + moduleVersionType);
        }
        return version;
    }

    private static String buildPseudoVer(RevCommit commit) {
        int commitTime = commit.getCommitTime();
        String version;
        String formattedDateTime = Instant.ofEpochSecond(commitTime, 0).atZone(ZoneOffset.UTC).format(formatter);
        version = String.format("v0.0.0-%s-%s", formattedDateTime, commit.abbreviate(12).name());
        return version;
    }

    private InputStream zip() {

        try {
            RevCommit revCommit = null;
            if (ModuleVersionType.PseudoVer.equals(moduleVersionType)) {
                revCommit = getRevCommitWithPseudoVer();
            }
            if (ModuleVersionType.SemVer.equals(moduleVersionType) || ModuleVersionType.Branch.equals(moduleVersionType)) {
                revCommit = getRevCommitWithSemVer();
            }

            if (revCommit == null) {
                //todo 自定义异常
                throw new RuntimeException("RevCommit not found");
            }
            return getZip(revCommit);
        } catch (Exception e) {
            throw new RuntimeException("process '.zip' file Exception", e);
        }
    }

    private InputStream mod() {

        try {
            RevCommit commit = null;
            if (ModuleVersionType.PseudoVer.equals(moduleVersionType)) {
                commit = getRevCommitWithPseudoVer();
            }
            if (ModuleVersionType.SemVer.equals(moduleVersionType) || ModuleVersionType.Branch.equals(moduleVersionType)) {
                commit = getRevCommitWithSemVer();
            }

            if (commit == null) {
                //todo 自定义异常
                throw new RuntimeException("not found");
            }

            return getGoMod(commit);
        } catch (Exception e) {
            throw new RuntimeException("process '.mod' file Exception", e);
        }
    }

    private InputStream doInitTarget() throws Exception {


        if (RepositoryFiles.isArtifact(repositoryPath)) {
            GoCoordinates artifactCoordinates = (GoCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
            moduleName = artifactCoordinates.getName();
            moduleVersion = artifactCoordinates.getVersion();
            fileType = artifactCoordinates.getExtension();
        } else {
            String path = RepositoryFiles.relativizePath(repositoryPath);
            initArtifactAttributes(path);
        }

        Supplier<InputStream> stringInputStreamFunction = fileTypeAndProcessorMap.get(fileType);
        if (stringInputStreamFunction == null) {
            throw new RuntimeException(String.format("File type .%s is not supported", fileType));
        }

        initGitCredentialsProvider();
        gitUrl = "https://" + moduleName + ".git";
        isRemoteRepositoryValid(gitUrl, credentialsProvider);
        initGitLocalRepo();
        initSemverRefMap();
        initBranchRefMap();
        initVersionType();
        return stringInputStreamFunction.get();
    }

    private void initVersionType() {
        if (moduleVersion == null) {
            moduleVersionType = ModuleVersionType.Unknown;
            return;
        }

        if (branchRefMap.containsKey(moduleVersion)) {
            moduleVersionType = ModuleVersionType.Branch;
            return;
        }

        boolean isSemVerType = semverRefMap.keySet().stream().anyMatch(version -> {
            return moduleVersion.equals("v" + version);
        });
        if (isSemVerType) {
            moduleVersionType = ModuleVersionType.SemVer;
            return;
        }

        if (pseudoVersionPattern.matcher(moduleVersion).matches()) {
            moduleVersionType = ModuleVersionType.PseudoVer;
            return;
        }

        throw new RuntimeException("initVersionType failed. Unable to determine VersionType");
    }

    private void initArtifactAttributes(String path) {

        if (path.endsWith("/@v/list")) {
            moduleName = path.substring(0, path.length() - "/@v/list".length());
            fileType = "list";
        } else if (path.endsWith("/@latest")) {
            moduleName = path.substring(0, path.length() - "/@latest".length());
            fileType = "latest";
        } else if (path.endsWith(".mod") || path.endsWith(".info")) {
            String[] parts = path.split("/@");
            Assert.isTrue(parts.length == 2, String.format("Illegal artifact path [%s]", path));

            moduleName = parts[0];
            String after = parts[1];
            Assert.isTrue(after.startsWith("v/"), String.format("Illegal artifact path [%s]", path));
            // remove 'v/'
            after = after.substring(2);

            fileType = FileUtil.extName(after);
            String suffix = "." + fileType;

            moduleVersion = after.substring(0, after.lastIndexOf(suffix));
        } else {
            throw new IllegalStateException(String.format("Illegal artifact path [%s]", path));
        }
    }

    private void initGitCredentialsProvider() {
        GoRepositoryConfigurationData repositoryConfiguration = (GoRepositoryConfigurationData) repositoryPath.getRepository().getRepositoryConfiguration();
        if (Objects.isNull(repositoryConfiguration)) {
            return;
        }
        List<Map<String, String>> gitVCSList = repositoryConfiguration.getGitVCS();
        if (gitVCSList == null) {
            return;
        }
        LinkedHashMap<String, Map<String, String>> gitVCSCredentials = new LinkedHashMap<>();
        for (Map<String, String> gitVCS : gitVCSList) {
            String url = gitVCS.get("url");
            if (StringUtils.isBlank(url)) {
                continue;
            }
            gitVCSCredentials.putIfAbsent(url, gitVCS);
        }
        String domain;
        int index = moduleName.indexOf("/");
        if (index == -1) {
            domain = moduleName;
        } else {
            domain = moduleName.substring(0, index);
        }
        Map<String, String> stringStringMap = gitVCSCredentials.get(domain);
        if (stringStringMap != null) {
            String username = stringStringMap.get("username");
            String password = stringStringMap.get("password");
            logger.info("found credential for domain {}, credential username:{}", domain, username);
            credentialsProvider = new UsernamePasswordCredentialsProvider(username, password);
        }
        logger.info("not found credential for domain {}", domain);

    }

    private Git getGit(File file) throws IOException, GitAPIException {
        StoredConfig config;
        Git git = FileUtil.isNotEmpty(file) ? Git.open(file) : Git.init()
                .setBare(true)
                .setDirectory(file)
                .call();
        config = git.getRepository().getConfig();
        config.setString("remote", "origin", "url", gitUrl);
        config.save();
        return git;
    }

    public static void proxyJgit() throws GitAPIException {
        CredentialsProvider credentialsProvider = new UsernamePasswordCredentialsProvider("1138827104@qq.com", "AAAqqq111...");
        Git.lsRemoteRepository()
                .setTransportConfigCallback(transport -> {
                    if (transport instanceof TransportHttp) {
                        // 设置代理
                        ((TransportHttp) transport).setHttpConnectionFactory(new JDKHttpConnectionFactory() {
                            @Override
                            public HttpConnection create(URL url) throws IOException {
                                return super.create(url);
                            }

                            @Override
                            public HttpConnection create(URL url, Proxy proxy) throws IOException {
                                Proxy specificProxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress("localhost", 50055));

                                return super.create(url, specificProxy);
                            }
                        });
                    }
                })
                .setRemote("https://jihulab.com/1138827104/go-hello-world-privete.git")
                .setCredentialsProvider(credentialsProvider)
                .call();
    }

    public void isRemoteRepositoryValid(String remoteUrl, CredentialsProvider credentialsProvider) throws Exception {
        Git.lsRemoteRepository()
                .setRemote(remoteUrl)
                .setCredentialsProvider(credentialsProvider)
                .call();
    }

    private void initSemverRefMap() throws IOException {
        List<Ref> refs = git.getRepository().getRefDatabase().getRefsByPrefix(Constants.R_TAGS);
        //Semantic Versioning  semverRef
        semverRefMap = new HashMap<>();
        for (Ref ref : refs) {
            String tag = ref.getName().replaceAll(Constants.R_TAGS, "");
            if (!tag.matches("v?\\d+\\.\\d+\\.\\d+.*")) {
                continue;
            }
            tag = tag.replaceFirst("^v", "");
            try {
                Version parse = Version.parse(tag);
                semverRefMap.put(parse, ref);
            } catch (Exception e) {
                logger.warn("not parse tag:{}", tag, e);
            }
        }
    }

    private void initBranchRefMap() throws IOException {
        List<Ref> refs = git.getRepository().getRefDatabase().getRefsByPrefix(Constants.R_REMOTES);
        //Semantic Versioning  semverRef
        branchRefMap = new HashMap<>();
        for (Ref ref : refs) {
            String branch = ref.getName().replaceAll(Constants.R_REMOTES + "origin/", "");
            branchRefMap.put(branch, ref);
        }
    }

    private Map<String, Ref> getRefs() throws GitAPIException {
        return git.lsRemote()
                .setCredentialsProvider(credentialsProvider)
                .setRemote("origin")
                .callAsMap();

    }

    private ByteArrayInputStream getInfoV2(String ver, RevCommit rev) {
        int commitTime = rev.getCommitTime();
        HashMap<String, Object> result = new LinkedHashMap<>();
        HashMap<String, String> Origin = new HashMap<>();

        Origin.put("VCS", "git");
        Origin.put("URL", gitUrl);
        Origin.put("Hash", rev.getId().getName());

        result.put("Version", ver);
        result.put("Time", Instant.ofEpochSecond(commitTime, 0).atZone(ZoneOffset.UTC).toString());
        result.put("Origin", Origin);
        String jsonString = JSONObject.toJSONString(result);
        return new ByteArrayInputStream(jsonString.getBytes());
    }

    private ByteArrayInputStream getInfo(Ref ref, RevCommit rev) {
        HashMap<String, Object> result = new LinkedHashMap<>();
        int commitTime = rev.getCommitTime();
        String version;
        if (CollectionUtil.isEmpty(semverRefMap)) {
            String formattedDateTime = Instant.ofEpochSecond(commitTime, 0).atZone(ZoneOffset.UTC).format(formatter);
            version = String.format("v0.0.0-%s-%s", formattedDateTime, rev.abbreviate(12).name());
        } else {
            String name = ref.getName();
            if (name.startsWith(Constants.R_TAGS)) {
                version = name.replaceAll(Constants.R_TAGS, "");
            } else if (name.startsWith(Constants.R_REMOTES)) {
                //分支返回语义化版本
                version = name.replace(Constants.R_REMOTES + "origin/", "");
                String formattedDateTime = Instant.ofEpochSecond(commitTime, 0).atZone(ZoneOffset.UTC).format(formatter);
                version = String.format("v0.0.0-%s-%s", formattedDateTime, rev.abbreviate(12).name());
            } else {
                throw new RuntimeException("unsupported ref name:" + name);
            }

        }

        HashMap<String, String> Origin = new HashMap<>();
        Origin.put("VCS", "git");
        Origin.put("URL", gitUrl);
        Origin.put("Hash", rev.getId().getName());

        result.put("Version", version);
        result.put("Time", Instant.ofEpochSecond(commitTime, 0).atZone(ZoneOffset.UTC).toString());
        result.put("Origin", Origin);
        String jsonString = JSONObject.toJSONString(result);
        return new ByteArrayInputStream(jsonString.getBytes());

    }

    private InputStream getZip(RevCommit commit) throws IOException {
        String directory = moduleName + "@" + moduleVersion + "/";
        RevTree tree = commit.getTree();

        try (TreeWalk treeWalk = new TreeWalk(git.getRepository())) {
            treeWalk.addTree(tree);
            treeWalk.setRecursive(true);

            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            ZipOutputStream zos = new ZipOutputStream(byteArrayOutputStream);

            while (treeWalk.next()) {
                ObjectId objectId = treeWalk.getObjectId(0);
                ObjectLoader loader = git.getRepository().open(objectId);
                // 目录
                ZipEntry ze = new ZipEntry(directory + treeWalk.getPathString());
                zos.putNextEntry(ze);

                byte[] bytes = loader.getBytes();
                zos.write(bytes, 0, bytes.length);
                zos.closeEntry();
            }
            zos.close();
            //todo 优化项，包装 ObjectLoader 直接返回InputStream
            return new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
        }
    }

    private InputStream getGoMod(RevCommit commit) throws IOException {
        // Get the tree in a commit, which represents the project's directory structure
        RevTree tree = commit.getTree();
        // The TreeWalk is used to traverse the Git tree
        try (TreeWalk treeWalk = new TreeWalk(git.getRepository())) {
            treeWalk.addTree(tree);
            // Traverse all the files in the tree
            treeWalk.setRecursive(true);
            // Set up the filter to find specific file
            treeWalk.setFilter(PathFilter.create("go.mod"));

            if (!treeWalk.next()) {
                throw new IllegalStateException("Did not find expected file 'go.mod'");
            }

            ObjectId objectId = treeWalk.getObjectId(0);
            ObjectLoader loader = git.getRepository().open(objectId);

            return loader.openStream();
        }
    }

    private RevCommit getRevCommitWithSemVer(Ref ref) throws IOException {
        RevCommit commit;
        if (ref == null) {
            return null;
        }
        ObjectId commitId = ref.getObjectId();
        //Browse the commit history of a Git repository using RevWalk
        commit = getRevCommitByCommitId(commitId);
        return commit;
    }

    private RevCommit getRevCommitWithSemVer() throws IOException {
        Ref ref = getRef();
        if (ref == null) {
            return null;
        }
        return getRevCommitWithSemVer(ref);
    }

    private RevCommit getRevCommitWithPseudoVer() throws IOException {
        RevCommit commit = null;
        String shortHash = moduleVersion.substring(moduleVersion.lastIndexOf("-") + 1);
        String versionAndTime = moduleVersion.replace("-" + shortHash, "");
        String dateTimeStr = versionAndTime.substring(versionAndTime.length() - 14);
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
        // 解析字符串到 LocalDateTime
        LocalDateTime dateTime = LocalDateTime.parse(dateTimeStr, formatter);
        // 将 LocalDateTime 转换为 epoch 秒
        long epochSecond = dateTime.toEpochSecond(ZoneOffset.UTC);

        Repository bareRepo = git.getRepository();
        try (RevWalk walk = new RevWalk(bareRepo)) {
            ObjectId resolve = null;
            try {
                resolve = bareRepo.resolve(shortHash);
            } catch (AmbiguousObjectException e) {
                //todo 处理短hash冲突的情况
                throw new RuntimeException(e);
            }
            commit = walk.parseCommit(resolve);
        }
        return commit;
    }


    private Ref getRef() {
        String branch = branchRefMap.keySet().stream().filter(version1 -> {
            return moduleVersion.equals(version1);
        }).findAny().orElse(null);
        if (branch != null) {
            return branchRefMap.get(branch);
        }

        Version ver = semverRefMap.keySet().stream().filter(version1 -> {
            String string = version1.toString();
            return moduleVersion.equals("v" + string);
        }).findAny().orElse(null);
        if (ver != null) {
            return semverRefMap.get(ver);
        }

        return null;
    }

    enum ModuleVersionType {
        Branch,
        SemVer,
        PseudoVer,
        Unknown
    }
}
