package com.veadan.folib.promotion;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.extra.spring.SpringUtil;
import com.alibaba.fastjson.JSON;
import com.veadan.folib.cloud.storage.s3fs.S3FileSystem;
import com.veadan.folib.cloud.storage.s3fs.S3Iterator;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.dto.PromotionArtifactDto;
import com.veadan.folib.dto.PromotionNodeOptionDto;
import com.veadan.folib.dto.TargetRepositoyDto;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.Consts;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.config.AuthSchemes;
import org.apache.http.client.config.CookieSpecs;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.HttpMultipartMode;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.apache.http.util.EntityUtils;
import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.file.FileDataBodyPart;
import org.glassfish.jersey.media.multipart.file.StreamDataBodyPart;
import org.glassfish.jersey.media.multipart.internal.MultiPartWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.*;
import java.util.concurrent.FutureTask;

/**
 * @author qijianping
 */
@Component
@Slf4j
public class PromotionUtil {

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Inject
    protected ConfigurationManagementService configurationManagementService;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Autowired
    private ThreadPoolTaskExecutor asyncRepositoryThreadPoolExecutor;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Async("asyncStorageThreadPoolExecutor")
    public void executeHanleCopy(String path, Repository destRepository, Repository srcRepository) {
        try {
            if (path.startsWith("s3://")) {
                handleS3ArtifactCopy(path, destRepository, srcRepository);
            } else {
                handleCopy(path, destRepository, srcRepository);
            }
            log.info("Artifact copyed [{}]", path);
        } catch (Exception e) {
            log.error("async handle copy artifact fail [{}]", e.getMessage());
        }

    }

    @Async("asyncStorageThreadPoolExecutor")
    public void executeHandleMove(ArtifactPromotion artifactPromotion) {
        final String srcStorageId = artifactPromotion.getSrcStorageId();
        final String srcRepositoryId = artifactPromotion.getSrcRepositoryId();

        Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);
        final RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());

        List<TargetRepositoyDto> list = artifactPromotion.getTargetRepositoyList();
        List<FutureTask<String>> listTask = new ArrayList<FutureTask<String>>();
        list.forEach(x -> {
            // 多个目标仓库移动
            String destStorageId = x.getTargetStorageId();
            String destRepositoryId = x.getTargetRepositoryId();
            Repository destRepository = repositoryManagementService.getStorage(destStorageId).getRepository(destRepositoryId);
            RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
            FutureTask<String> future = new FutureTask<String>(
                    new ArtifactPromotionCopyTask(srcPath.getTarget().toString(), destRepository, srcRepository));
            listTask.add(future);
            asyncRepositoryThreadPoolExecutor.submit(future);
        });
        boolean delFlag = true;
        for (FutureTask<String> task : listTask) {
            try {
                String rs = task.get();
                if (StringUtils.isNotBlank(rs)) {
                    delFlag = false;
                    log.error("Artitfact copy err {}", rs);
                }
            } catch (Exception e) {
                log.error("Exception {}", e.getMessage());
            }
        }
        if (delFlag) {
            try {
                artifactManagementService.delete(srcRepositoryPath, false);
            } catch (IOException e) {
                log.error("async handle move artifact fail [{}]", e.getMessage());
            }
        }
        log.info("Artifact moved [{}]", artifactPromotion.getPath());
    }

    /**
     * 获取绝对路径下的所有文件
     *
     * @param path path
     */
    public List<File> getNFSFiles(String path) {
        int fileNum = 0, folderNum = 0;
        File file = new File(path);
        LinkedList<File> list = new LinkedList<>();
        List<File> resultList = new ArrayList<>();

        if (file.exists()) {
            if (null == file.listFiles()) {
                resultList.add(file);
                return resultList;
            }
            list.addAll(Arrays.asList(file.listFiles()));
            while (!list.isEmpty()) {
                File[] files = list.removeFirst().listFiles();
                if (null == files) {
                    continue;
                }
                for (File f : files) {
                    if (f.isDirectory()) {
                        log.info("文件夹:{}", f.getAbsolutePath());
                        list.add(f);
                        folderNum++;
                    } else {
                        log.info("文件:{}", f.getAbsolutePath());
                        resultList.add(f);
                        fileNum++;
                    }
                }
            }
        } else {
            log.info("文件不存在!");
        }
        log.info("文件夹数量:{} ,文件数量:{}", folderNum, fileNum);
        return resultList;
    }

    public PromotionNodeOptionDto getPromotionPullDto(PromotionArtifactDto promotionArtifactDto) {
        PromotionNodeOptionDto promotionNodeOptionDto = new PromotionNodeOptionDto();
        promotionNodeOptionDto.setStorageId(promotionArtifactDto.getTargetStorageId());
        promotionNodeOptionDto.setRepostoryId(promotionArtifactDto.getTargetRepostoryId());
        Map<String, File> filePathMap = new HashMap<>();

        int fileNum = 0, folderNum = 0;
        File file = new File(promotionArtifactDto.getPath());
        LinkedList<File> list = new LinkedList<>();
        if (file.exists()) {
            if (null == file.listFiles()) {
                filePathMap.put(
                        getRelativePath(file.getAbsolutePath(),
                                promotionArtifactDto.getSrcStorageId(),
                                promotionArtifactDto.getSrcRepostoryId()), file);
                promotionNodeOptionDto.setPathMap(filePathMap);
                return promotionNodeOptionDto;
            }
            list.addAll(Arrays.asList(file.listFiles()));
            while (!list.isEmpty()) {
                File[] files = list.removeFirst().listFiles();
                if (null == files) {
                    continue;
                }
                for (File f : files) {
                    if (f.isDirectory()) {
                        log.info("文件夹:{}", f.getAbsolutePath());
                        list.add(f);
                        folderNum++;
                    } else {
                        log.info("文件:{}", f.getAbsolutePath());
                        filePathMap.put(getRelativePath(f.getAbsolutePath(),
                                promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId()), f);
                        fileNum++;
                    }
                }
            }
        } else {
            log.info("文件不存在!");
        }
        log.info("文件夹数量:{} ,文件数量:{}", folderNum, fileNum);
        promotionNodeOptionDto.setPathMap(filePathMap);
        return promotionNodeOptionDto;
    }

    public PromotionNodeOptionDto getPromotionUploadDto(PromotionArtifactDto promotionArtifactDto) {
        PromotionNodeOptionDto promotionNodeOptionDto = new PromotionNodeOptionDto();
        promotionNodeOptionDto.setStorageId(promotionArtifactDto.getTargetStorageId());
        promotionNodeOptionDto.setRepostoryId(promotionArtifactDto.getTargetRepostoryId());
        Map<String, File> filePathMap = new HashMap<>();

        int fileNum = 0, folderNum = 0;
        File file = new File(promotionArtifactDto.getPath());
        LinkedList<File> list = new LinkedList<>();

        if (file.exists()) {
            if (null == file.listFiles()) {
                filePathMap.put(
                        getRelativePath(file.getAbsolutePath(),
                                promotionArtifactDto.getSrcStorageId(),
                                promotionArtifactDto.getSrcRepostoryId()), file);
                promotionNodeOptionDto.setPathMap(filePathMap);
                return promotionNodeOptionDto;
            }
            list.addAll(Arrays.asList(file.listFiles()));
            while (!list.isEmpty()) {
                File[] files = list.removeFirst().listFiles();
                if (null == files) {
                    continue;
                }
                for (File f : files) {
                    if (f.isDirectory()) {
                        log.info("文件夹:{}", f.getAbsolutePath());
                        list.add(f);
                        folderNum++;
                    } else {
                        log.info("文件:{}", f.getAbsolutePath());
                        filePathMap.put(getRelativePath(f.getAbsolutePath(),
                                promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId()), f);
                        fileNum++;
                    }
                }
            }
        } else {
            log.info("文件不存在!");
        }
        log.info("文件夹数量:{} ,文件数量:{}", folderNum, fileNum);
        promotionNodeOptionDto.setPathMap(filePathMap);
        return promotionNodeOptionDto;
    }

    private String getRelativePath(String absolutePath, String storageId, String repostoryId) {
        String temp = storageId + "/" + repostoryId;
        int fPathIndex = absolutePath.lastIndexOf(temp + File.separator);
        return absolutePath.substring(fPathIndex, absolutePath.length()).replace(temp + File.separator, "");
    }


    public void handleCopy(String path, Repository destRepository, Repository srcRepository) throws Exception {
        List<File> list = getNFSFiles(path);
        for (File file : list) {
            try (InputStream is = Files.newInputStream(file.toPath());) {
                String fPath = file.getAbsolutePath().toString();
                int fPathIndex = fPath.lastIndexOf(srcRepository.getId() + File.separator);
                String temp = fPath.substring(fPathIndex, fPath.length()).replace(srcRepository.getId() + File.separator, "");
                RepositoryPath destPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), temp);
                artifactManagementService.store(destPath, is);
            } catch (IOException e) {
                e.printStackTrace();
                throw new Exception(e.getMessage());
            }
        }
    }

    public void handleS3ArtifactCopy(String path, Repository destRepository, Repository srcRepository) throws Exception {
        S3Path s3Path = new S3Path(SpringUtil.getBean(S3FileSystem.class), path);
        List<S3Path> s3FilesPaths = getS3FiePaths(s3Path);
        for (S3Path s3FilePath : s3FilesPaths) {
            log.info("s3FilePath {} upload start", s3FilePath);
            String fPath = s3FilePath.toString();
            int fPathIndex = fPath.lastIndexOf(srcRepository.getId() + File.separator);
            String temp = fPath.substring(fPathIndex, fPath.length()).replace(srcRepository.getId() + File.separator, "");
            RepositoryPath uploadPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), temp);
            try (InputStream is = Files.newInputStream(s3FilePath);) {
                artifactManagementService.store(uploadPath, is);
            } catch (IOException e) {
                e.printStackTrace();
                throw new Exception(e.getMessage());
            }
        }
        s3FilesPaths.clear();
    }

    public List<String> getFileRelativePaths(RepositoryPath repositoryPath) throws Exception {
        String repositoryId = repositoryPath.getRepository().getId();
        String storageId = repositoryPath.getRepository().getStorage().getId();
        String absolutePath = repositoryPath.toAbsolutePath().toString();
        List<String> list = new ArrayList<String>();
        if (absolutePath.contains("s3://")) {
            S3Path s3Path = new S3Path(SpringUtil.getBean(S3FileSystem.class), repositoryPath.getTarget().toString());
            List<S3Path> s3FilesPaths = getS3FiePaths(s3Path);
            for (S3Path file : s3FilesPaths) {
                String filePathStr = file.toAbsolutePath().toString();
                int indexTemp = filePathStr.indexOf(storageId + "/" + repositoryId);
                String temp = filePathStr.
                        substring(indexTemp + (storageId + "/" + repositoryId).length(), filePathStr.length());
                if (temp.startsWith("/")) {
                    temp = temp.substring(1, temp.length());
                }
                list.add(temp);
            }
        } else {
            List<File> files = getNFSFiles(absolutePath);
            for (File file : files) {
                String fileAbsolutePath = file.getAbsolutePath();
                int indexTemp = fileAbsolutePath.indexOf(storageId + "/" + repositoryId);
                String temp = fileAbsolutePath.
                        substring(indexTemp + (storageId + "/" + repositoryId).length(), fileAbsolutePath.length());
                if (temp.startsWith("/")) {
                    temp = temp.substring(1, temp.length());
                }
                list.add(temp);
            }
        }
        return list;
    }


    public static String getBucket(String path) {
        return path.replace("s3://", "").split("/")[1];
    }

    public static String getS3Uri(String path) {
        String[] array = path.replace("s3://", "").split("/");
        StringBuilder stringBuilder = new StringBuilder();
        List<String> list = Arrays.asList(array);
        for (int i = 0; i < list.size(); i++) {
            if (i == 0) {
                continue;
            }
            if (i + 1 == list.size()) {
                stringBuilder.append(list.get(i));
            } else {
                stringBuilder.append(list.get(i)).append(File.separator);
            }

        }
        return stringBuilder.toString();
    }

    public List<S3Path> getS3FiePaths(S3Path s3Path) throws Exception {
        List<S3Path> listFile = new ArrayList<S3Path>();
        List<S3Path> listDir = new ArrayList<S3Path>();

        S3Iterator s3Iterator = new S3Iterator(s3Path);
        while (s3Iterator.hasNext()) {
            S3Path s3PathTemp = s3Iterator.next();
            if (s3PathTemp.getFileAttributes() == null || s3PathTemp.getFileAttributes().isDirectory()) {
                listDir.add(s3PathTemp);
            } else {
                listFile.add(s3PathTemp);
            }
        }
        while (listDir.size() != 0) {
            S3Path currentPath = listDir.get(0);
            listDir.remove(currentPath);
            s3Iterator = new S3Iterator(currentPath);
            while (s3Iterator.hasNext()) {
                S3Path s3PathTemp = s3Iterator.next();
                if (s3PathTemp.getFileAttributes() == null || s3PathTemp.getFileAttributes().isDirectory()) {
                    listDir.add(s3PathTemp);
                } else {
                    log.info("s3 file {}", s3PathTemp);
                    listFile.add(s3PathTemp);
                }
            }
        }
        log.info("s3Path [{}]  文件数量：{}", s3Path.toUri().toString(), listFile.size());
        return listFile;
    }


    /**
     * 以流的形式文件上传
     *
     * @param host
     * @param restPath
     * @param fileName
     * @param inputStream
     * @return java.lang.String
     */
    public static String uploadFileByInputStream(String host, String restPath,
                                                 String fileName, InputStream inputStream) throws Exception {
        String result = "";
        try {
            //构建HttpClient对象,此处主要是为了支持https请求，如若只是请求http，可通过HttpClients.createDefault();获取httpClient对象
            HttpClient client = wrapClient(host, restPath);
            //CloseableHttpClient client = HttpClients.createDefault();
            //构建POST请求
            HttpPost httpPost = new HttpPost(host + restPath);
            httpPost.setHeader("X-Atlassian-Token", "nocheck");
            //此处设置的是confluence的账号密码，也可根据需求修改为设置cookies
            httpPost.setHeader("Authorization", "");

            httpPost.setConfig(setTimeOutConfig(httpPost.getConfig()));
            //处理文件后面的setMode是用来解决文件名称乱码的问题:以浏览器兼容模式运行，防止文件名乱码。
            MultipartEntityBuilder builder = MultipartEntityBuilder.create()
                    .setMode(HttpMultipartMode.BROWSER_COMPATIBLE);

            builder.setCharset(Charset.forName("UTF-8"));
            //如果以inputStream形式上传文件，接收文件上传的接口可能会限制content-type为二进制：ContentType.DEFAULT_BINARY，否则为ContentType.MULTIPART_FORM_DATA
            builder.addBinaryBody("file", inputStream, ContentType.DEFAULT_BINARY, fileName);

            HttpEntity httpEntity = builder.build();
            httpPost.setEntity(httpEntity);
            HttpResponse httpResponse = client.execute(httpPost);
            HttpEntity responseEntity = httpResponse.getEntity();
            //判断响应结果
            if (responseEntity != null && httpResponse.getStatusLine().getStatusCode() != 200) {
                throw new Exception("客户端请求失败！");
            } else {
                log.info("文件上传响应结果" + JSON.toJSONString(httpResponse.getEntity()));
                ;
//                result = HttpUtils.parseString(httpResponse);
                System.out.println("文件上传响应结果：" + result);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }

        return result;
    }

    /**
     * 获取 HttpClient
     *
     * @param host
     * @param path
     * @return org.apache.http.client.HttpClient
     */
    public static HttpClient wrapClient(String host, String path) {
        HttpClient httpClient = HttpClientBuilder.create().build();
        if (host != null && host.startsWith("https://")) {
            return sslClient();
        } else if (StringUtils.isBlank(host) && path != null && path.startsWith("https://")) {
            return sslClient();
        }
        return httpClient;
    }


    /**
     * 在调用SSL之前需要重写验证方法，取消检测SSL,创建ConnectionManager，添加Connection配置信息
     * 此httpClient支持https
     *
     * @return org.apache.http.impl.client.CloseableHttpClient
     */
    private static CloseableHttpClient sslClient() {
        try {
            // 在调用SSL之前需要重写验证方法，取消检测SSL
            X509TrustManager trustManager = new X509TrustManager() {
                @Override
                public X509Certificate[] getAcceptedIssuers() {
                    return null;
                }

                @Override
                public void checkClientTrusted(X509Certificate[] xcs, String str) {
                }

                @Override
                public void checkServerTrusted(X509Certificate[] xcs, String str) {
                }
            };
            SSLContext ctx = SSLContext.getInstance(SSLConnectionSocketFactory.TLS);
            ctx.init(null, new TrustManager[]{trustManager}, null);
            SSLConnectionSocketFactory socketFactory = new SSLConnectionSocketFactory(ctx, NoopHostnameVerifier.INSTANCE);
            // 创建Registry
            RequestConfig requestConfig = RequestConfig.custom().setCookieSpec(CookieSpecs.STANDARD_STRICT)
                    .setExpectContinueEnabled(Boolean.TRUE).setTargetPreferredAuthSchemes(Arrays.asList(AuthSchemes.NTLM, AuthSchemes.DIGEST))
                    .setProxyPreferredAuthSchemes(Arrays.asList(AuthSchemes.BASIC)).build();
            Registry<ConnectionSocketFactory> socketFactoryRegistry = RegistryBuilder.<ConnectionSocketFactory>create()
                    .register("http", PlainConnectionSocketFactory.INSTANCE)
                    .register("https", socketFactory).build();
            // 创建ConnectionManager，添加Connection配置信息
            PoolingHttpClientConnectionManager connectionManager = new PoolingHttpClientConnectionManager(socketFactoryRegistry);
            CloseableHttpClient closeableHttpClient = HttpClients.custom().setConnectionManager(connectionManager)
                    .setDefaultRequestConfig(requestConfig).build();
            return closeableHttpClient;
        } catch (KeyManagementException ex) {
            throw new RuntimeException(ex);
        } catch (NoSuchAlgorithmException ex) {
            throw new RuntimeException(ex);
        }
    }

    public static RequestConfig setTimeOutConfig(RequestConfig requestConfig) {
        if (requestConfig == null) {
            requestConfig = RequestConfig.DEFAULT;
        }
        return RequestConfig.copy(requestConfig)
                .setConnectionRequestTimeout(60000)
                .setConnectTimeout(60000)
                .setSocketTimeout(10000)
                .build();
    }

    /**
     * 以post方式调用第三方接口,以form-data 形式  发送 MultipartFile 文件数据
     *
     * @param url       post请求url
     * @param uploadDto 晋级上传参数实体
     * @return string
     */
    public String upload(String url, PromotionNodeOptionDto uploadDto) throws Exception {

        // 创建Http实例
        CloseableHttpClient httpClient = HttpClients.createDefault();
        // 创建HttpPost实例
        HttpPost httpPost = new HttpPost(url);

        // 请求参数配置
        RequestConfig requestConfig = RequestConfig.custom().setSocketTimeout(60000).setConnectTimeout(60000)
                .setConnectionRequestTimeout(10000).build();
        httpPost.setConfig(requestConfig);

        try {
            MultipartEntityBuilder builder = MultipartEntityBuilder.create();
            builder.setCharset(java.nio.charset.Charset.forName("UTF-8"));
            builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
            HashMap<String, String> filePathMap = new HashMap<String, String>();
            uploadDto.getPathMap().forEach((x, y) -> {
                String fileName = y.getAbsolutePath();
                builder.addBinaryBody("files", y, ContentType.MULTIPART_FORM_DATA, fileName);
                filePathMap.put(fileName, x);
            });
            //表单中其他参数
            builder.addPart("storageId", new StringBody(uploadDto.getStorageId(), ContentType.create("text/plain", Consts.UTF_8)));
            builder.addPart("repostoryId", new StringBody(uploadDto.getRepostoryId(), ContentType.create("text/plain", Consts.UTF_8)));
            builder.addPart("filePathMap", new StringBody(JSON.toJSONString(filePathMap), ContentType.create("text/plain", Consts.UTF_8)));

            HttpEntity entity = builder.build();
            httpPost.setEntity(entity);// todo HttpAuthenticationFeature
            HttpResponse response = httpClient.execute(httpPost);// 执行提交


            if (response.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
                // 返回
                String res = EntityUtils.toString(response.getEntity(), java.nio.charset.Charset.forName("UTF-8"));
                return res;
            }

        } catch (Exception e) {
            e.printStackTrace();
            log.error("晋级上传失败 {}", e.getMessage());
            throw new Exception("upload fail");
        } finally {
            if (httpClient != null) {
                try {
                    httpClient.close();
                } catch (IOException e) {
                    log.error("关闭HttpPost连接失败！");
                }
            }
        }
        return "error";
    }

}
