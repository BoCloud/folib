package com.veadan.folib.artifact.coordinates;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSONObject;
import com.github.zafarkhaja.semver.Version;
import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.providers.io.AbsFallbackRemoteArtifactInputStream;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.yaml.configuration.repository.GoRepositoryConfigurationData;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.errors.IncorrectObjectTypeException;
import org.eclipse.jgit.errors.MissingObjectException;
import org.eclipse.jgit.lib.*;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevTree;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.transport.CredentialsProvider;
import org.eclipse.jgit.transport.FetchResult;
import org.eclipse.jgit.transport.RefSpec;
import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider;
import org.eclipse.jgit.treewalk.TreeWalk;
import org.eclipse.jgit.treewalk.filter.PathFilter;
import org.jetbrains.annotations.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.net.URI;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author pengYongQiang
 * @date 1/15/2024 9:45
 */

public class GoFallbackRemoteArtifactInputStream extends AbsFallbackRemoteArtifactInputStream {
    private static final Logger logger = LoggerFactory.getLogger(GoFallbackRemoteArtifactInputStream.class);
    private String moduleName;
    private String fileType;
    private String moduleVersion;
    private String gitUrl;
    private final RepositoryPath repositoryPath;
    private CredentialsProvider credentialsProvider;
    private final HashMap<String, Supplier<InputStream>> fileTypeAndProcessorMap = new HashMap<>();

    // 创建一个日期格式化器，匹配 Go 中的时间格式
    private final static DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");

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

    private InputStream latest() {
        Git git = null;
        try {
            //todo  primary https , then http

            git = initGit2();
            Collection<Ref> refs = getRefs(credentialsProvider, git);
            Map<String, Ref> refsV2 = getRefsV2(credentialsProvider, git);

            //Semantic Versioning
            Ref ref = null;
            Map<Version, Ref> map = semverRef(refs);
            // 没有符合语义化版本的tag，取最新提交
            if (CollectionUtil.isEmpty(map)) {
                ref = refsV2.get("HEAD");
            } else {
                List<Version> sortedVersion = map.keySet().stream().sorted().collect(Collectors.toList());
                Version ver = sortedVersion.get(sortedVersion.size() - 1);
                if (ver == null) {
                    throw new RuntimeException("version is not found");
                }
                ref = map.get(ver);
            }

            return getInfo2(git, credentialsProvider, ref);
        } catch (Exception e) {
            throw new RuntimeException("init git Exception", e);
        }
    }

    private InputStream list() {

        Git git = null;
        try {
            //todo  primary https , then http
            git = initGit2();
            Collection<Ref> refs = getRefs(credentialsProvider, git);
            //Semantic Versioning
            Map<Version, Ref> map = semverRef(refs);
            if (CollectionUtil.isEmpty(map)) {
                return new ByteArrayInputStream("".getBytes());
            }
            List<String> sortedVersion = map.keySet().stream().sorted()
                    .map(version1 -> {
                        return "v" + version1.toString();
                    }).collect(Collectors.toList());
            String join = String.join("\n", sortedVersion);
            return new ByteArrayInputStream(join.getBytes());
        } catch (Exception e) {
            throw new RuntimeException("init git Exception", e);
        }
    }

    private InputStream info() {

        Git git = null;
        try {
            //todo  primary https , then http

            git = initGit2();
            Collection<Ref> refs = getRefs(credentialsProvider, git);
            //Semantic Versioning
            Map<Version, Ref> map = semverRef(refs);

            Version ver = map.keySet().stream().filter(version1 -> {
                String string = version1.toString();
                return moduleVersion.equals("v" + string);
            }).findAny().orElse(null);
            if (ver == null) {
                throw new RuntimeException("version is not found");
            }
            Ref ref = map.get(ver);
            return getInfo2(git, credentialsProvider, ref);
        } catch (Exception e) {
            throw new RuntimeException("init git Exception", e);
        }
    }

    private InputStream zip() {

        Git git = null;
        try {
            //todo  primary https , then http
            git = initGit2();
            Collection<Ref> refs = getRefs(credentialsProvider, git);
            //Semantic Versioning
            Map<Version, Ref> map = semverRef(refs);

            Version ver = map.keySet().stream().filter(version1 -> {
                String string = version1.toString();
                return moduleVersion.equals("v" + string);
            }).findAny().orElse(null);
            if (ver == null) {
                throw new RuntimeException("version is not found");
            }
            Ref ref = map.get(ver);
            return getZip(git, ref, moduleName + "@" + moduleVersion + "/");
        } catch (Exception e) {
            throw new RuntimeException("init git Exception", e);
        }
    }

    private InputStream mod() {

        Git git = null;
        try {
            //todo  primary https , then http
            git = initGit2();
            Collection<Ref> refs = getRefs(credentialsProvider, git);
            //Semantic Versioning
            Map<Version, Ref> map = semverRef(refs);

            Version ver = map.keySet().stream().filter(version1 -> {
                String string = version1.toString();
                return moduleVersion.equals("v" + string);
            }).findAny().orElse(null);
            if (ver == null) {
                throw new RuntimeException("version is not found");
            }
            Ref ref = map.get(ver);

            return getGoMod(git, ref, credentialsProvider);
        } catch (Exception e) {
            throw new RuntimeException("init git Exception", e);
        }
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

    private InputStream doInitTarget() {
        try {
            if (RepositoryFiles.isArtifact(repositoryPath)) {
                InitArtifactCoordinates();
            } else if (RepositoryFiles.isMetadata(repositoryPath)) {
                InitMetadata();
            }
        } catch (IOException e) {
            throw new RuntimeException(String.format("init fail,repositoryPath:%s", repositoryPath.toString()));
        }

        InitCredentialsProvider();
        InitGitUrl();


        Supplier<InputStream> stringInputStreamFunction = fileTypeAndProcessorMap.get(fileType);
        if (stringInputStreamFunction == null) {
            throw new RuntimeException(String.format("File type .%s is not supported", fileType));
        }
        return stringInputStreamFunction.get();
    }

    private void InitGitUrl() {
        gitUrl = "https://" + moduleName + ".git";
    }

    private void InitMetadata() throws IOException {
        String path = RepositoryFiles.relativizePath(repositoryPath);
        if (path.endsWith("/@v/list")) {
            moduleName = path.substring(0, path.length() - "/@v/list".length());
            fileType = "list";
        } else if (path.endsWith("/@latest")) {
            moduleName = path.substring(0, path.length() - "/@latest".length());
            fileType = "latest";
        } else {
            throw new IllegalStateException(String.format("Metadata Path [%s] is not supported", path));
        }
    }

    private void InitArtifactCoordinates() {
        try {
            GoArtifactCoordinates artifactCoordinates = (GoArtifactCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
            moduleName = artifactCoordinates.getName();
            moduleVersion = artifactCoordinates.getVersion();
            fileType = artifactCoordinates.getExtension();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }


    private void InitCredentialsProvider() {
        GoRepositoryConfigurationData repositoryConfiguration = (GoRepositoryConfigurationData) repositoryPath.getRepository().getRepositoryConfiguration();
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

        String domain = moduleName.substring(0, moduleName.indexOf("/"));
        Map<String, String> stringStringMap = gitVCSCredentials.get(domain);
        if (stringStringMap != null) {
            String username = stringStringMap.get("username");
            String password = stringStringMap.get("password");
            credentialsProvider = new UsernamePasswordCredentialsProvider(username, password);
            logger.info("found credential for domain {}, credential username:{}", domain, username);
        } else {
            logger.info("not found credential for domain {}", domain);
        }

    }


    private static boolean isGitRepository(File directory) {
        return FileUtil.isNotEmpty(directory);
    }


    private Git initGit2() throws Exception {
        String relativize = repositoryPath.relativize().toString();
        relativize = relativize.substring(0, relativize.lastIndexOf("@"));
        String s = repositoryPath.getFileSystem().getTempPath().getTarget() + "\\vcsCache\\" + relativize;

        File file = new File(s);
        StoredConfig config;

        Git git = isGitRepository(file) ? Git.open(file) : Git.init()
                .setBare(true)
                .setDirectory(file)
                .call();
        config = git.getRepository().getConfig();
        config.setString("remote", "origin", "url", gitUrl);
        config.save();
        return git;
    }

    public static void main(String[] args) throws Exception {
        //github.com/pengyongqiang666/hello-world-go-private/
        String repoUrl = "https://jihulab.com/1138827104/go-hello-world-privete.git"; // 替换为目标仓库的 URI
        CredentialsProvider credentialsProvider = new UsernamePasswordCredentialsProvider("1138827104@qq.com", "AAAqqq111...");
        GoFallbackRemoteArtifactInputStream goFallbackRemoteArtifactInputStream = new GoFallbackRemoteArtifactInputStream(null);
        goFallbackRemoteArtifactInputStream.initGit(repoUrl, credentialsProvider);
        //gettags(repoUrl);
    }

    private void initGit(String repoUrl, CredentialsProvider credentialsProvider) throws Exception {

        File file = new File("D:\\tmp\\gitdemo2");
        StoredConfig config;


        try (Git git = isGitRepository(file) ? Git.open(file) : Git.init()
                .setBare(true)
                .setDirectory(file)
                .call()) {
            config = git.getRepository().getConfig();
            config.setString("remote", "origin", "url", repoUrl);
            config.save();


            Collection<Ref> refs = getRefs(credentialsProvider, git);
            Map<String, Ref> refsV2 = getRefsV2(credentialsProvider, git);
            Ref ref = refsV2.get("HEAD");
            //Semantic Versioning
            Map<Version, Ref> map = semverRef(refs);

            Repository repository = git.getRepository();
            try (RevWalk walk = new RevWalk(repository)) {
                ObjectId head = repository.resolve("HEAD");

                // Now you can use latestCommit to access commit details
            }


            List<Version> sortedVersion = map.keySet().stream().sorted().collect(Collectors.toList());
            // Version ver = sortedVersion.get(sortedVersion.size() - 1);
            Version ver = map.keySet().stream().filter(version1 -> {
                String string = version1.toString();
                return "v1.0.1".equals("v" + string);
            }).findAny().orElse(null);
            if (ver == null) {
                throw new RuntimeException("version is not found");
            }


            Ref latestVersionRef = map.get(ver);
            gitFetch(git, credentialsProvider, latestVersionRef.getName());

            //    InputStream info = getInfo2(git, credentialsProvider, latestVersionRef);

            //     InputStream goMod = getGoMod(git, latestVersionRef, credentialsProvider);
            URI uri = new URI(repoUrl);

            InputStream zip = getZip(git, latestVersionRef, "prefix2/");


//
//            InputStream latest = getLatest(git, latestVersionRef);

        }

    }

    private static Map<Version, Ref> semverRef(Collection<Ref> refs) {
        //Semantic Versioning  semverRef
        Map<Version, Ref> map = new HashMap<>();
        for (Ref ref : refs) {
            String tag = ref.getName().replaceAll("refs/tags/", "");
            if (!tag.matches("v?\\d+\\.\\d+\\.\\d+.*")) {
                continue;
            }
            tag = tag.replaceFirst("^v", "");
            try {
                Version parse = Version.parse(tag);
                map.put(parse, ref);
            } catch (Exception e) {
                logger.warn("not parse tag:{}", tag, e);
            }
        }
        return map;
    }

    private static Collection<Ref> getRefs(CredentialsProvider credentialsProvider, Git git) throws GitAPIException {
        Collection<Ref> call = null;
        call = git.lsRemote()
                .setCredentialsProvider(credentialsProvider)
                .setRemote("origin")
                .call();

        return call;
    }

    private static Map<String, Ref> getRefsV2(CredentialsProvider credentialsProvider, Git git) throws GitAPIException {
        return git.lsRemote()
                .setCredentialsProvider(credentialsProvider)
                .setRemote("origin")
                .callAsMap();

    }


    @Nullable
    private ByteArrayInputStream getInfo2(Git git, CredentialsProvider credentialsProvider, Ref latestVersionRef) throws GitAPIException, IncorrectObjectTypeException, MissingObjectException {
        ObjectId objectId = latestVersionRef.getObjectId();
        String name = latestVersionRef.getName();
        Iterable<RevCommit> logs = null;
        try {
            logs = git.log()
                    .add(objectId)
                    .setMaxCount(1)
                    .call();
        } catch (MissingObjectException e) {
            FetchResult origin = gitFetch(git, credentialsProvider, name);
            logs = git.log()
                    .add(objectId)
                    .setMaxCount(1)
                    .call();
        }
        Iterator<RevCommit> iterator = logs.iterator();
        if (iterator.hasNext()) {
            RevCommit rev = iterator.next();
            HashMap<String, Object> result = new LinkedHashMap<>();

            String version;
            if ("HEAD".equals(name)) {
                String formattedDateTime = LocalDateTime.now().format(formatter);
                version = String.format("v0.0.0-%s-%s", formattedDateTime, rev.abbreviate(12).name());
            } else {
                version = name.replaceAll("refs/tags/", "");
            }

            HashMap<String, String> Origin = new HashMap<>();
            Origin.put("VCS", "git");
            Origin.put("URL", gitUrl);
            Origin.put("Hash", rev.getId().getName());

            result.put("Version", version);
            result.put("Time", Instant.ofEpochSecond(rev.getCommitTime(), 0).atZone(ZoneOffset.UTC).toString());
            result.put("Origin", Origin);
            String jsonString = JSONObject.toJSONString(result);
            return new ByteArrayInputStream(jsonString.getBytes());
        }

        return null;
    }

    private static FetchResult gitFetch(Git git, CredentialsProvider credentialsProvider, String tag) throws GitAPIException {
        return git.fetch()
                .setCredentialsProvider(credentialsProvider)
                .setRemote("origin")
                // .setRefSpecs(new RefSpec("+refs/tags/v1.0.3:refs/tags/v1.0.3"))
                .setRefSpecs(new RefSpec(String.format("+%s:%s", tag, tag)))
                .setDepth(1)
                .setForceUpdate(true)
                .call();
    }

    private static InputStream getZip(Git git, Ref latestVersionRef, String directory) throws IOException {
        try (RevWalk revWalk = new RevWalk(git.getRepository())) {
            ObjectId commitId = latestVersionRef.getObjectId();
            RevCommit commit = revWalk.parseCommit(commitId);
            RevTree tree = commit.getTree();

            try (TreeWalk treeWalk = new TreeWalk(git.getRepository())) {
                treeWalk.addTree(tree);
                treeWalk.setRecursive(true);

//                FileOutputStream fos = new FileOutputStream("archivettt.zip");
//                ZipOutputStream zos = new ZipOutputStream(fos);
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
    }

    private static InputStream getGoMod(Git git, Ref ref, CredentialsProvider credentialsProvider) throws IOException {

        ObjectId commitId = ref.getObjectId();
        //Browse the commit history of a Git repository using RevWalk
        try (RevWalk revWalk = new RevWalk(git.getRepository())) {
            // Parse a specific commit
            RevCommit commit = null;
            try {
                commit = revWalk.parseCommit(commitId);
            } catch (IOException e) {
                try {
                    gitFetch(git, credentialsProvider, ref.getName());
                } catch (GitAPIException ex) {
                    throw new RuntimeException(ex);
                }
            }
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

                ObjectStream objectStream = loader.openStream();
                return objectStream;
            }
        }
    }

    private static void gettags(String repoUrl) {
        try {

            CredentialsProvider cp = new UsernamePasswordCredentialsProvider("1138827104@qq.com", "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");
            // 使用 Git.lsRemoteRepository() 获取远程仓库信息
            Collection<Ref> refs = Git.lsRemoteRepository()
                    .setCredentialsProvider(cp)
                    .setHeads(false)
                    .setTags(true)
                    .setRemote(repoUrl)
                    .call();

            // 打印所有的 tag
            for (Ref ref : refs) {
                if (ref.getPeeledObjectId() != null) {
                    // 获取注释标签的实际对象 ID
                    System.out.println("Annotated tag: " + ref + " " + ref.getPeeledObjectId());
                } else {
                    // 获取轻量级标签
                    System.out.println("Lightweight tag: " + ref);
                    String tag = ref.getName().substring("refs/tags/".length());
                    System.out.println(tag);
                }
            }

        } catch (GitAPIException e) {
            e.printStackTrace();
        }
    }


}
