package com.veadan.folib.controllers.layout.cocoapods;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.StreamProgress;
import cn.hutool.http.HttpUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.artifact.coordinates.CocoapodsArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.util.CocoapodsArtifactUtil;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.archivers.zip.ZipFile;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/***
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/2 14:58
 * @since x.x.x
 */
@RestController
@LayoutRequestMapping(CocoapodsArtifactCoordinates.LAYOUT_NAME)
public class CocoapodsIndexController
        extends BaseArtifactController {

    //    @PreAuthorize("hasAuthority('MANAGEMENT_REBUILD_INDEXES')")
    @GetMapping(value = "/{storageId}/{repositoryId}/index/fetchIndex")
    public ResponseEntity repoArtIndex(@RepositoryMapping Repository repository, HttpServletRequest request, HttpServletResponse response) throws IOException {
        final String type = repository.getType();

        ResponseEntity responseEntity = null;
        if (type.equals(RepositoryTypeEnum.HOSTED.getType()))
        { responseEntity = this.repoArtIndexHosted(repository, response); }
        else if (type.equals(RepositoryTypeEnum.PROXY.getType()))
        { responseEntity = this.repoArtIndexProxy(repository, response);}
        else
        { return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("invalid repository type"); }
        if (null != responseEntity)
        { return responseEntity; }

        return new ResponseEntity<>("ok", HttpStatus.OK);
    }
    
    private ResponseEntity repoArtIndexHosted(Repository repository, HttpServletResponse response)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        final RepositoryPath repositoryPath;
        try 
        { repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, ".specs/"); } 
        catch (IOException e) 
        { return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage()); }
        final Path repositoryPathTarget = repositoryPath.getTarget();
        final String baseUrl = super.getBaseUrl();

        ServletOutputStream servletOutputStream = null;
        TarArchiveOutputStream tarArchiveOutputStream = null;
        try
        {
            response.setHeader("Content-Disposition", "attachment;filename=file.tar.gz");
            response.setContentType("application/x-gzip");

            final String indexFolder = repositoryPathTarget.toUri().getPath();
            servletOutputStream = response.getOutputStream();
            tarArchiveOutputStream = new TarArchiveOutputStream(new GzipCompressorOutputStream(servletOutputStream));
            tarArchiveOutputStream.setLongFileMode(TarArchiveOutputStream.LONGFILE_GNU);
            this.tarGzFolder(repository, baseUrl, indexFolder, indexFolder, tarArchiveOutputStream);
            servletOutputStream.flush();
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
        finally {
            try {
                if (null != tarArchiveOutputStream)
                { tarArchiveOutputStream.close(); }
                if (null != servletOutputStream)
                { servletOutputStream.close(); }
            } catch (Exception e) {
                logger.error("关闭zip文件流失败", e);
            }
        }
        
        return null;
    }

    private final Pattern PROXY_REPO_URL_PATTERN = Pattern.compile("http(?:s)?\\:.*/(.*?)\\.zip");
    
    private final AtomicReference<CompletableFuture<Void>> DOWNLOAD_COCOAPODS_PROXY_INDEX_TASK = new AtomicReference<>();
    
    private ResponseEntity repoArtIndexProxy(Repository repository, HttpServletResponse response)
    {
        final CompletableFuture<Void> recentlyTask = DOWNLOAD_COCOAPODS_PROXY_INDEX_TASK.get();
        if (null != recentlyTask && !recentlyTask.isDone())
        {
            return ResponseEntity
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("目前代理正在缓存中，请稍后再试");
        }
        
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        final RemoteRepository remoteRepository = repository.getRemoteRepository();
        final String url = remoteRepository.getUrl();
        final String baseUrl = super.getBaseUrl();
        final String username = remoteRepository.getUsername();
        final String password = remoteRepository.getPassword();

        final Matcher matcher = PROXY_REPO_URL_PATTERN.matcher(url);
        if (!matcher.find())
        {
            return ResponseEntity
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("代理仓库地址不正确");
        }
        final String fileName = matcher.group(1);
        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, String.format(".specs/%s.tar.gz", fileName));
        final File proxyTarGzFile = repositoryPath.getTarget().toFile();

        final CompletableFuture<Void> downloadCocoapodsProxyIndexTask = CompletableFuture.runAsync(() -> {
            final long currentTimeMillis = System.currentTimeMillis();
            final File tmpDir = FileUtil.getTmpDir();
            final File tempZipFile = FileUtil.createTempFile(String.format("cocoapods-index-%s", currentTimeMillis), ".zip", tmpDir, true);
//            final File tempTarGzFile = FileUtil.createTempFile(String.format("cocoapods-index-%s", currentTimeMillis), ".tar.gz", tmpDir, true);

            final StreamProgress streamProgress = new StreamProgress() {
                @Override
                public void start() {
                    logger.info("开始下载Cocoapods仓库（{}-{}）代理（{}）！！！", storageId, repositoryId, url);
                }
                @Override
                public void progress(long l) {
                    
                }
                @Override
                public void finish() {
                    logger.info("结束下载Cocoapods仓库（{}-{}）代理（{}）！！！", storageId, repositoryId, url);
                }
            };
            try {
                HttpUtil.download(url, Files.newOutputStream(tempZipFile.toPath()), true, streamProgress);
                logger.info("开始转换Cocoapods仓库代理仓库Zip（{}）", tempZipFile.toPath());
                this.zip2Targz(baseUrl, tempZipFile.getPath(), proxyTarGzFile.getPath());
                logger.info("结束转换Cocoapods仓库代理仓库Zip（{}）", tempZipFile.toPath());
                
///                logger.info("开始调用validateAndStore存储Cocoapods仓库代理仓库TarGz（{}）", tempTarGzFile.toPath());
///                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, String.format(".specs/%s.tar.gz", fileName));
///                artifactManagementService.validateAndStore(repositoryPath, FileUtil.getInputStream(tempTarGzFile));
///                logger.info("结束调用validateAndStore存储Cocoapods仓库代理仓库TarGz（{}）", tempTarGzFile.toPath());
            } catch (Exception e) {
                logger.info("下载Cocoapods远程索引失败");
                throw new RuntimeException(e);
            }
            
            // 删除临时文件
            if (tempZipFile.exists())
            { 
                tempZipFile.delete();
                logger.info("删除cocoapods仓库代理仓库Zip（{}）", tempZipFile.toPath());
            }
//            if (tempTarGzFile.exists())
//            { 
//                tempTarGzFile.delete(); 
//                logger.info("删除cocoapods仓库代理仓库TarGz（{}）", tempTarGzFile.toPath());
//            }
        });
        downloadCocoapodsProxyIndexTask.thenRun(() -> {
            DOWNLOAD_COCOAPODS_PROXY_INDEX_TASK.set(null);
            logger.info("Cocoapods仓库（{}-{}）代理初始化成功", storageId, repositoryId);
        }).exceptionally((e) -> {
            e.printStackTrace();
            DOWNLOAD_COCOAPODS_PROXY_INDEX_TASK.set(null);
            logger.error("Cocoapods仓库（{}-{}）代理初始化发生异常", storageId, repositoryId, e);
            return null;
        }).join();
        DOWNLOAD_COCOAPODS_PROXY_INDEX_TASK.set(downloadCocoapodsProxyIndexTask);

        response.setHeader("Content-Disposition", "attachment;filename=file.tar.gz");
        response.setContentType("application/x-gzip");
        try (final ServletOutputStream outputStream = response.getOutputStream();
             final FileInputStream fileInputStream = new FileInputStream(proxyTarGzFile);
        ){
            int len = 0;
            byte[] buffer = new byte[1024];
            while((len=fileInputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, len);
            }
        } catch (IOException e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
        return ResponseEntity.ok("代理缓存任务已经成功提交，请稍后进行拉取代理仓库");
    }

    private void tarGzFolder(Repository repository, String baseUrl, String rootPath, String srcFolder, TarArchiveOutputStream archiveOutputStream) throws Exception 
    {
        final File folder = new File(srcFolder);
        //遍历文件夹下所有的文件和文件夹
        final File[] files = folder.listFiles();
        
        if (null != files)
        {
            for (File file : files) 
            {
                //如果是文件夹,递归压缩
                if (file.isDirectory()) {
                    tarGzFolder(repository, baseUrl, rootPath, file.getAbsolutePath(), archiveOutputStream);
                    continue;
                }
    
                if (file.getName().endsWith(".podspec"))
                {
                    byte[] bytes = FileUtil.readBytes(file);
                    final String podspecFileUri = file.getAbsolutePath().replace(rootPath, StringUtils.EMPTY);
                    
                    final RepositoryPath repositoryPath = artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), String.format(".specs/%s", podspecFileUri));
                    if (repositoryPath != null  && null != repositoryPath.getArtifactEntry())
                    { // 获取到有Pod源代码路径地址则替换
                        final Artifact artifactEntry = repositoryPath.getArtifactEntry();
                        final String path = artifactEntry.getArtifactCoordinates().getPath();
                        final String newSourceUrl = String.format("%s/%s%s", baseUrl, "storages", path);
                        final String newPodspecContent = CocoapodsArtifactUtil.replaceNewSourceUrlOfPodspecContent(new String(bytes), newSourceUrl);
                        if (StringUtils.isNotBlank(newPodspecContent))
                        { bytes = newPodspecContent.getBytes(StandardCharsets.UTF_8); }
                    }

                    final TarArchiveEntry entry = new TarArchiveEntry(podspecFileUri);
                    entry.setSize(bytes.length);
                    archiveOutputStream.putArchiveEntry(entry);
                    archiveOutputStream.write(bytes);
                    archiveOutputStream.closeArchiveEntry();
                }
            }
        }
    }

    private static final Pattern POD_REPO_GIT_URL_PATTERN = Pattern.compile("http(?:s)?://.*?/(.*?)/(.*?)\\.git");

    private void zip2Targz(String baseUrl, String zipPath, String targzPath) throws IOException
    {
        FileUtil.touch(targzPath);
        // 创建tar.gz输出流
        try (final FileOutputStream fos = new FileOutputStream(targzPath);
             final GzipCompressorOutputStream gos = new GzipCompressorOutputStream(fos);
             final TarArchiveOutputStream tos = new TarArchiveOutputStream(gos);

             final FileInputStream fis = new FileInputStream(zipPath);
             final ZipInputStream zis = new ZipInputStream(fis);
             final ByteArrayOutputStream baos = new ByteArrayOutputStream();)
        {
            tos.setLongFileMode(TarArchiveOutputStream.LONGFILE_GNU);
            final JSONObject podNewSourceObj = new JSONObject();

            ZipEntry ze;
            while ((ze = zis.getNextEntry()) != null) {
                String name = ze.getName();
                // 删除一层结构
                name = name.substring(name.indexOf("/")+1);
                if (StringUtils.isEmpty(name))
                { continue; }
                
                final TarArchiveEntry tarArchiveEntry = new TarArchiveEntry(name);
                if (ze.isDirectory()) {
                    // 目录处理逻辑
                    tos.putArchiveEntry(tarArchiveEntry);
                } else {
                    // 文件处理逻辑
                    byte[] buffer = new byte[1024];
                    int readLen = -1;
                    baos.reset();
                    while ((readLen = zis.read(buffer)) != -1) {
                        baos.write(buffer, 0, readLen);
                    }
                    
                    byte[] extra = baos.toByteArray();
                    if (name.endsWith(".podspec.json"))
                    { // 将资源下载指向folib
                        // http://localhost:38080/storages/lcoal-mdo/zzzz-coccoapods-proxy
                        final String podSpecJson = new String(extra, StandardCharsets.UTF_8);
                        final JSONObject podJsonObj = JSON.parseObject(podSpecJson);
                        final String version = podJsonObj.getString("version");
                        final JSONObject sourceObj = podJsonObj.getJSONObject("source");
                        if (null != sourceObj && sourceObj.containsKey("git") && StringUtils.isNotBlank(version))
                        {
                            final String podRepoGitUrl = sourceObj.getString("git");
                            final Matcher matcher = POD_REPO_GIT_URL_PATTERN.matcher(podRepoGitUrl);
                            if (matcher.find())
                            {
                                final String owner = matcher.group(1);
                                final String podName = matcher.group(2);
                                final String newSourceUrl = String.format("%s/pod/git/%s/%s/%s", baseUrl, owner, podName, version);
                                podNewSourceObj.clear();
                                podNewSourceObj.put("http", newSourceUrl);
                                podNewSourceObj.put("type", "tgz");
                                podJsonObj.put("source", podNewSourceObj);
                                extra = JSON.toJSONString(podJsonObj, true).getBytes(StandardCharsets.UTF_8);
                            }
                            else
                            { /*logger.info("非法PodGitUrl：{}", podRepoGitUrl);*/ }
                        }
                        else
                        { /*logger.info("非法PodSource信息：{}", JSON.toJSONString(sourceObj));*/ }
                    }

                    tarArchiveEntry.setSize(extra.length);
                    tos.putArchiveEntry(tarArchiveEntry);
                    tos.write(extra);
                }
                tos.closeArchiveEntry();
            }
        }
    }


//    public static void main(String[] args) throws IOException {
////        final long l = System.currentTimeMillis();
//        new CocoapodsIndexController().zip2Targz("http://127.0.0.1:8081/proxy/dada", "/Users/zerowang/Downloads/demo/Specs-master.zip", "/Users/zerowang/Downloads/demo/Specs-master.tar.gz");
////        System.out.println(System.currentTimeMillis() - l + " ms");
////        traverseZip2("/Users/zerowang/Downloads/demo/Specs-master.tar.gz");
//
////        System.out.println("=========================");
////
////        traverse("/Users/zerowang/Downloads/demo/Specs-master.tar.gz");
////        traverse("/Users/zerowang/.cocoapods/repos-art/CocoaPods-Proxy/file.tgz", true);
////        System.out.println("=========================");
////        traverse("/Users/zerowang/Downloads/demo/Specs-master.tar.gz", true);
//
//        final String s = "Specs-master/";
//        System.out.println(s.substring(s.indexOf("/")+1));
//    }
//
//    public static void traverseZip(String zipFilePath) throws IOException {
//
//        ZipFile zipFile = new ZipFile(zipFilePath);
//
//        Enumeration<? extends ZipEntry> entries = zipFile.getEntries();
//        int i = 0;
//        while(entries.hasMoreElements()){
//            ZipEntry entry = entries.nextElement();
//            String name = entry.getName();
//            System.out.println("zip: " + name);
//            if (++i >= 10)
//            { break; }
//        }
//
//        zipFile.close();
//    }
//
//    public static void traverseZip2(String zipFilePath) throws IOException {
//
//        FileInputStream fis = new FileInputStream(zipFilePath);
//        ZipInputStream zis = new ZipInputStream(fis);
//        ByteArrayOutputStream baos = new ByteArrayOutputStream();
//
//
//        ZipEntry ze;
//        int i = 0;
//        while ((ze = zis.getNextEntry()) != null) {
//
//            String name = ze.getName();
//            long size = ze.getSize();
//
//            System.out.println("zip: " + name);
//            
////            if (ze.isDirectory()) {
////                // 目录处理逻辑
////            } else {
////                // 文件处理逻辑
////
////                byte[] buffer = new byte[1024];
////                int readLen = -1;
////                baos.reset();
////                while ((readLen = zis.read(buffer)) != -1) {
////                    baos.write(buffer, 0, readLen);
////                }
////                String content = new String(baos.toByteArray(), StandardCharsets.UTF_8);
////                System.out.println(content);
////                // TODO: 处理文件内容
////                System.out.println("------------------");
////            }
//            if (++i >= 10)
//            { break; }
//        }
//
//        zis.close();
//    }
//    
//    public static void traverse(String tarGzPath, boolean printFirst10) throws IOException {
//
//        InputStream is = new FileInputStream(tarGzPath);
//        GzipCompressorInputStream gzIn = new GzipCompressorInputStream(is);
//        TarArchiveInputStream tarIn = new TarArchiveInputStream(gzIn);
//
//        TarArchiveEntry entry;
//        int i = 0;
//        final ArrayList<String> strings = new ArrayList<>();
//        while ((entry = tarIn.getNextTarEntry()) != null) {
//            // 处理每个条目
//            String name = entry.getName();
//            long size = entry.getSize();
//            if (printFirst10)
//            {
//                System.out.println("tarGz: " + name);
//                if (++i >= 10)
//                { break; }
//            }
//            else
//            {
//                if (StringUtils.isNotBlank(name) && name.contains(".podspec.json"))
//                {
//                    try {
//                        // 读取文件内容
//                        byte[] content = new byte[(int) size];
//                        tarIn.read(content, 0, (int) size);
//                        final String str = new String(content);
//                        final JSONObject jsonObject = JSON.parseObject(str);
//                        final JSONObject source = jsonObject.getJSONObject("source");
//                        if (null != source)
//                        {
//                            final String key = source.keySet().stream().sorted().collect(Collectors.joining(","));
//                            if (!strings.contains(key))
//                            {
//                                System.out.println("-------------------------------");
//                                System.out.println();
//                                strings.add(key);
//                                System.out.printf("其他sourceInfo: %s%n", str);
//                            }
//                        }
//                        else
//                        {
//                            System.out.println("-------------------------------");
//                            System.out.println();
//                            System.out.printf("不包含sourceInfoPod：%s%n", str);
//                        }
//                    }catch (Exception e)
//                    {
//                        e.printStackTrace();
//                        System.out.printf("文件读取发送异常文件：%s%n", name);
//                    }
//                }
//            }
//        }
//
//        tarIn.close();
//        gzIn.close();
//        is.close();
//    }
}
