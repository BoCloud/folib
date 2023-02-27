package com.veadan.folib.promotion;

import cn.hutool.core.io.FileUtil;
import cn.hutool.extra.spring.SpringUtil;
import com.alibaba.fastjson.JSON;
import com.veadan.folib.cloud.storage.s3fs.S3FileSystem;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.domain.PromotionFileRelativePath;
import com.veadan.folib.dto.PromotionArtifactDto;
import com.veadan.folib.dto.PromotionNodeOptionDto;
import com.veadan.folib.dto.TargetRepositoyDto;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.*;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.RepositoryPathUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.HttpClient;
import org.apache.http.client.config.AuthSchemes;
import org.apache.http.client.config.CookieSpecs;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.glassfish.jersey.media.multipart.Boundary;
import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.file.StreamDataBodyPart;
import org.glassfish.jersey.media.multipart.internal.MultiPartWriter;
import org.springframework.beans.factory.annotation.Autowired;
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
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.*;
import java.util.concurrent.FutureTask;
import java.util.stream.Collectors;

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

    @Autowired
    private ArtifactService artifactService;

    @Autowired
    private ArtifactWebService artifactWebService;

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

    public PromotionNodeOptionDto getPromotionUploadDto(PromotionArtifactDto promotionArtifactDto) throws Exception {
        PromotionNodeOptionDto promotionNodeOptionDto = new PromotionNodeOptionDto();
        promotionNodeOptionDto.setStorageId(promotionArtifactDto.getTargetStorageId());
        promotionNodeOptionDto.setRepostoryId(promotionArtifactDto.getTargetRepostoryId());
        Map<String, Map<String, InputStream>> filePathMap = new HashMap<>();
        Map<String, Object> fileMetaDataMap = new HashMap<>();
        if (promotionArtifactDto.getPath().startsWith("s3://")) {
            s3PromotionUpload(promotionArtifactDto, filePathMap, fileMetaDataMap);
        } else {
            nfsPromotionUpload(promotionArtifactDto, filePathMap, fileMetaDataMap);
        }
        promotionNodeOptionDto.setPathMap(filePathMap);
        promotionNodeOptionDto.setFileMetaDataMap(fileMetaDataMap);
        return promotionNodeOptionDto;
    }

    private void s3PromotionUpload(PromotionArtifactDto promotionArtifactDto, Map<String, Map<String, InputStream>> filePathMap, Map<String, Object> fileMetaDataMap) throws Exception {
        S3Path s3Path = new S3Path(SpringUtil.getBean(S3FileSystem.class), promotionArtifactDto.getPath());
        List<S3Path> s3FilesPaths = RepositoryPathUtil.getS3FiePaths(s3Path);
        for (S3Path s3FilePath : s3FilesPaths) {
            Map<String, InputStream> inputStreamMap = new HashMap<>();
            inputStreamMap.put(s3FilePath.toAbsolutePath().toString(), Files.newInputStream(s3FilePath));
            String relativePath = getRelativePath(s3FilePath.toAbsolutePath().toString(),
                    promotionArtifactDto.getSrcStorageId(),
                    promotionArtifactDto.getSrcRepostoryId());
            filePathMap.put(relativePath, inputStreamMap);

            // 添加跨节点的元数据同步
            RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), relativePath);
            fileMetaDataMap.put(relativePath, getMetaData(srcRepositoryPath));
        }
        // 判断是否是docker 版本路径的复制
        String absolutePath = promotionArtifactDto.getPath();
        String tempStr = promotionArtifactDto.getSrcStorageId() + File.separator + promotionArtifactDto.getSrcRepostoryId() + File.separator;
        int fPathIndex = absolutePath.lastIndexOf(tempStr);
        String relativizePath = absolutePath.substring(fPathIndex, absolutePath.length()).replace(tempStr, "");
        RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), relativizePath);
        boolean isDockerVersion = srcRepositoryPath.getRepository().getLayout().equalsIgnoreCase("docker") && s3FilesPaths.size() == 2;
        if (isDockerVersion) {
            //  blobs
            String[] arrayPath = relativizePath.split(File.separator);
            List<S3Path> fileContents = s3FilesPaths.stream().filter(file -> !(file.toString().endsWith(".sha256"))).collect(Collectors.toList());  //+propertiesBooter.getStorageBooterBasedir()+"/"+propertiesBooter.getVaultDirectory() + "/storages/"
            S3Path file = fileContents.get(0);
            String manifestString = Files.readString(file);
            ImageManifest manifest = JSON.parseObject(manifestString, ImageManifest.class);
            List<String> layerList = manifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
            String manifestConfig = manifest.getConfig().getDigest();
            layerList.add(manifestConfig);
            for (String layer : layerList) {
                String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                String blobSha256 = blob + ".sha256";
                RepositoryPath vSrcBlobPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), blob);
                RepositoryPath vSrcBlobSha256Path = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), blobSha256);
                Map<String, InputStream> inputStreamMapBlobPath = new HashMap<>();
                Map<String, InputStream> inputStreamMapSha256Path = new HashMap<>();
                inputStreamMapBlobPath.put(vSrcBlobPath.getTarget().toAbsolutePath().toString(), Files.newInputStream(vSrcBlobPath));
                inputStreamMapSha256Path.put(vSrcBlobSha256Path.getTarget().toAbsolutePath().toString(), Files.newInputStream(vSrcBlobSha256Path));
                filePathMap.put(blob, inputStreamMapBlobPath);
                filePathMap.put(blobSha256, inputStreamMapSha256Path);
            }
            //  manifest
            String[] array = file.getKey().split(File.separator);
            String mainfestFile = arrayPath[0] + File.separator + "manifest" + File.separator + array[array.length - 1];
            String mainfestFileSha256 = mainfestFile + ".sha256";
            String[] mainfestFileAarry = new String[]{mainfestFile, mainfestFileSha256};
            for (String mainfestFileStr : mainfestFileAarry) {
                RepositoryPath srcMainfestPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), mainfestFileStr);
                Map<String, InputStream> inputStreamMapMainfestPath = new HashMap<>();
                inputStreamMapMainfestPath.put(srcMainfestPath.getTarget().toAbsolutePath().toString(), Files.newInputStream(srcMainfestPath));
                filePathMap.put(mainfestFileStr, inputStreamMapMainfestPath);
            }
        }
    }

    private void nfsPromotionUpload(PromotionArtifactDto promotionArtifactDto, Map<String, Map<String, InputStream>> filePathMap, Map<String, Object> fileMetaDataMap) throws IOException {
        List<File> list = RepositoryPathUtil.getNFSFiles(promotionArtifactDto.getPath());
        for (File file : list) {
            Map<String, InputStream> inputStreamMap = new HashMap<>();
            inputStreamMap.put(file.getAbsolutePath(), Files.newInputStream(file.toPath()));
            String relativePath = getRelativePath(file.getAbsolutePath(),
                    promotionArtifactDto.getSrcStorageId(),
                    promotionArtifactDto.getSrcRepostoryId());
            filePathMap.put(relativePath, inputStreamMap);
            // 添加跨节点的元数据同步
            RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), relativePath);
            fileMetaDataMap.put(relativePath, getMetaData(srcRepositoryPath));
        }
        // 判断是否是docker 版本路径的复制
        String absolutePath = promotionArtifactDto.getPath();
        String tempStr = promotionArtifactDto.getSrcStorageId() + File.separator + promotionArtifactDto.getSrcRepostoryId() + File.separator;
        int fPathIndex = absolutePath.lastIndexOf(tempStr);
        String relativizePath = absolutePath.substring(fPathIndex, absolutePath.length()).replace(tempStr, "");
        RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), relativizePath);
        boolean isDockerVersion = srcRepositoryPath.getRepository().getLayout().equalsIgnoreCase("docker") && list.size() == 2;
        if (isDockerVersion) {
            //  blobs
            String[] arrayPath = relativizePath.split(File.separator);
            List<File> fileContents = list.stream().filter(file -> !(file.getName().endsWith(".sha256"))).collect(Collectors.toList());
            File file = fileContents.get(0);
            String manifestString = FileUtil.readString(srcRepositoryPath.toAbsolutePath() + File.separator + file.getName(), "UTF-8");
            ImageManifest manifest = JSON.parseObject(manifestString, ImageManifest.class);
            List<String> layerList = manifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
            String manifestConfig = manifest.getConfig().getDigest();
            layerList.add(manifestConfig);
            for (String layer : layerList) {
                String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                String blobSha256 = blob + ".sha256";
                RepositoryPath vSrcBlobPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), blob);
                RepositoryPath vSrcBlobSha256Path = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), blobSha256);
                Map<String, InputStream> inputStreamMapBlobPath = new HashMap<>();
                Map<String, InputStream> inputStreamMapSha256Path = new HashMap<>();
                inputStreamMapBlobPath.put(vSrcBlobPath.getTarget().toAbsolutePath().toString(), Files.newInputStream(vSrcBlobPath));
                inputStreamMapSha256Path.put(vSrcBlobSha256Path.getTarget().toAbsolutePath().toString(), Files.newInputStream(vSrcBlobSha256Path));
                filePathMap.put(blob, inputStreamMapBlobPath);
                filePathMap.put(blobSha256, inputStreamMapSha256Path);
            }
            //  manifest
            String mainfestFile = arrayPath[0] + File.separator + "manifest" + File.separator + file.getName();
            String mainfestFileSha256 = arrayPath[0] + File.separator + "manifest" + File.separator + file.getName() + ".sha256";
            String[] mainfestFileAarry = new String[]{mainfestFile, mainfestFileSha256};
            for (String mainfestFileStr : mainfestFileAarry) {
                RepositoryPath srcMainfestPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), mainfestFileStr);
                Map<String, InputStream> inputStreamMapMainfestPath = new HashMap<>();
                inputStreamMapMainfestPath.put(srcMainfestPath.getTarget().toAbsolutePath().toString(), Files.newInputStream(srcMainfestPath));
                filePathMap.put(mainfestFileStr, inputStreamMapMainfestPath);
            }
        }
    }

    private String getRelativePath(String absolutePath, String storageId, String repostoryId) {
        String temp = storageId + "/" + repostoryId;
        int fPathIndex = absolutePath.lastIndexOf(temp + File.separator);
        return absolutePath.substring(fPathIndex, absolutePath.length()).replace(temp + File.separator, "");
    }


    public void handleCopy(String path, Repository destRepository, Repository srcRepository) throws Exception {
        List<File> list = RepositoryPathUtil.getNFSFiles(path);
        for (File file : list) {
            String fPath = file.getAbsolutePath().toString();
            String tempStr = srcRepository.getStorage().getId() + File.separator + srcRepository.getId() + File.separator;
            int fPathIndex = fPath.lastIndexOf(tempStr);
            String temp = fPath.substring(fPathIndex, fPath.length()).replace(tempStr, "");
            RepositoryPath destPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), temp);
            log.debug("temp {}   destPath {}", temp, destPath.toString());
            boolean isDocker = srcRepository.getLayout().equalsIgnoreCase("docker");
            if (isDocker) {
                if (!file.getName().contains("sha256") && !file.getName().endsWith(".sha256")) {
                    Files.copy(Paths.get(file.getAbsolutePath()), destPath);
                    continue;
                }
            }
            try (InputStream is = Files.newInputStream(file.toPath());) {
                artifactManagementService.store(destPath, is);
                // 同步metadata
                RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), temp);
                setMetaData(destPath, getMetaData(srcPath));
            } catch (IOException e) {
                e.printStackTrace();
                throw new Exception(e.getMessage());
            }
        }
        // 判断是否是docker 版本路径的复制
        boolean isDockerVersion = srcRepository.getLayout().equalsIgnoreCase("docker") && list.size() == 2;
        if (isDockerVersion) {
            // copy blobs manifest
            String tempStr = srcRepository.getStorage().getId() + File.separator + srcRepository.getId() + File.separator;
            int fPathIndex = path.lastIndexOf(tempStr);
            String relativizePath = path.substring(fPathIndex, path.length()).replace(tempStr, "");
            String[] arrayPath = relativizePath.split(File.separator);
            if (arrayPath.length != 2) {
                return;
            }
//            RepositoryPath repositoryPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), relativizePath);
            List<File> fileContents = list.stream().filter(file -> !(file.getName().endsWith(".sha256"))).collect(Collectors.toList());
            File file = fileContents.get(0);
            String manifestString = Files.readString(file.toPath());
//            String manifestString = FileUtil.readString(repositoryPath.toAbsolutePath() + File.separator + file.getName(), "UTF-8");
            ImageManifest manifest = JSON.parseObject(manifestString, ImageManifest.class);
            List<String> layerList = manifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
            String manifestConfig = manifest.getConfig().getDigest();
            // copy blobs
            layerList.add(manifestConfig);
            for (String layer : layerList) {
                String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                String blobSha256 = blob + ".sha256";
                RepositoryPath vSrcBlobPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), blob);
                RepositoryPath vSrcBlobSha256Path = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), blobSha256);
                try (InputStream blobIs = Files.newInputStream(vSrcBlobPath);
                     InputStream blobSha256Is = Files.newInputStream(vSrcBlobSha256Path);
                ) {
                    RepositoryPath destBlobPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), blob);
                    RepositoryPath destBlobSha256Path = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), blobSha256);
                    log.info("destBlobPath {}  destBlobSha256Path {}",destBlobPath.toString(),destBlobSha256Path.toString());
                    artifactManagementService.store(destBlobPath, blobIs);
                    artifactManagementService.store(destBlobSha256Path, blobSha256Is);
                } catch (Exception e) {
                    e.printStackTrace();
                    log.error("{} blob copy error {}", relativizePath, e.getMessage());
                }
            }
            //  copy manifest
            String mainfestFile = arrayPath[0] + File.separator + "manifest" + File.separator + file.getName();
            String mainfestFileSha256 = mainfestFile + ".sha256";
            String[] mainfestFileAarry = new String[]{mainfestFile, mainfestFileSha256};
            for (String mainfestFileStr : mainfestFileAarry) {
                RepositoryPath srcMainfestPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), mainfestFileStr);
                try (InputStream mainfestIs = Files.newInputStream(srcMainfestPath)) {
                    RepositoryPath destBlobPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), mainfestFileStr);
                    artifactManagementService.store(destBlobPath, mainfestIs);
                } catch (Exception e) {
                    e.printStackTrace();
                    log.error("{} manifest copy error {}", relativizePath, e.getMessage());
                }
            }
        }
    }

    public void handleS3ArtifactCopy(String path, Repository destRepository, Repository srcRepository) throws Exception {
        S3Path s3Path = new S3Path(SpringUtil.getBean(S3FileSystem.class), path);
        List<S3Path> s3FilesPaths = RepositoryPathUtil.getS3FiePaths(s3Path);
        for (S3Path s3FilePath : s3FilesPaths) {
            log.info("s3FilePath {} copy start", s3FilePath);
            String fPath = s3FilePath.toString();
            String tempStr = srcRepository.getStorage().getId() + File.separator + srcRepository.getId() + File.separator;
            int fPathIndex = fPath.lastIndexOf(tempStr);
            String temp = fPath.substring(fPathIndex, fPath.length()).replace(tempStr, "");
            RepositoryPath uploadPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), temp);
            try (InputStream is = Files.newInputStream(s3FilePath);) {
                artifactManagementService.store(uploadPath, is);
                // 同步metadata
                RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), temp);
                setMetaData(uploadPath, getMetaData(srcPath));
            } catch (IOException e) {
                e.printStackTrace();
                throw new Exception(e.getMessage());
            }
        }
        // 判断是否是docker 版本路径的复制
        boolean isDockerVersion = srcRepository.getLayout().equalsIgnoreCase("docker") && s3FilesPaths.size() == 2;
        if (isDockerVersion) {
            // copy blobs manifest
            String tempStr = srcRepository.getStorage().getId() + File.separator + srcRepository.getId() + File.separator;
            int fPathIndex = path.lastIndexOf(tempStr);
            String relativizePath = path.substring(fPathIndex, path.length()).replace(tempStr, "");
            String[] arrayPath = relativizePath.split(File.separator);
            if (arrayPath.length != 2) {
                return;
            }
//            RepositoryPath repositoryPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), relativizePath);
            List<S3Path> fileContents = s3FilesPaths.stream().filter(file -> !(file.toAbsolutePath().endsWith(".sha256"))).collect(Collectors.toList());  //+propertiesBooter.getStorageBooterBasedir()+"/"+propertiesBooter.getVaultDirectory() + "/storages/"
            S3Path filePath = fileContents.get(0);
            String manifestString = Files.readString(filePath);
            ImageManifest manifest = JSON.parseObject(manifestString, ImageManifest.class);
            List<String> layerList = manifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
            String manifestConfig = manifest.getConfig().getDigest();
            // copy blobs
            layerList.add(manifestConfig);
            for (String layer : layerList) {
                String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                String blobSha256 = blob + ".sha256";
                RepositoryPath vSrcBlobPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), blob);
                RepositoryPath vSrcBlobSha256Path = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), blobSha256);
                try (InputStream blobIs = Files.newInputStream(vSrcBlobPath);
                     InputStream blobSha256Is = Files.newInputStream(vSrcBlobSha256Path);
                ) {
                    RepositoryPath destBlobPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), blob);
                    RepositoryPath destBlobSha256Path = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), blobSha256);
                    artifactManagementService.store(destBlobPath, blobIs);
                    artifactManagementService.store(destBlobSha256Path, blobSha256Is);
                } catch (Exception e) {
                    e.printStackTrace();
                    log.error("{} blob copy error {}", relativizePath, e.getMessage());
                }
            }
            // copy manifest
            String[] array = filePath.getKey().split(File.separator);
            String mainfestFile = arrayPath[0] + File.separator + "manifest" + File.separator + array[array.length - 1];
            String mainfestFileSha256 = mainfestFile + ".sha256";
            String[] mainfestFileAarry = new String[]{mainfestFile, mainfestFileSha256};
            for (String mainfestFileStr : mainfestFileAarry) {
                RepositoryPath srcMainfestPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), mainfestFileStr);
                try (InputStream mainfestIs = Files.newInputStream(srcMainfestPath)) {
                    RepositoryPath destBlobPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), mainfestFileStr);
                    artifactManagementService.store(destBlobPath, mainfestIs);
                } catch (Exception e) {
                    e.printStackTrace();
                    log.error("{} manifest copy error {}", relativizePath, e.getMessage());
                }
            }
        }

        s3FilesPaths.clear();
    }

    public PromotionFileRelativePath getFileRelativePaths(RepositoryPath repositoryPath, boolean isDockerVersionPath) throws Exception {
        String repositoryId = repositoryPath.getRepository().getId();
        String storageId = repositoryPath.getRepository().getStorage().getId();
        String absolutePath = repositoryPath.toAbsolutePath().toString();
        List<String> list = new ArrayList<String>();
        Map<String, Object> metaData = new HashMap<>();
        if (absolutePath.contains("s3://")) {
            S3Path s3Path = new S3Path(SpringUtil.getBean(S3FileSystem.class), repositoryPath.getTarget().toString());
            List<S3Path> s3FilesPaths = RepositoryPathUtil.getS3FiePaths(s3Path);
            for (S3Path file : s3FilesPaths) {
                String filePathStr = file.toAbsolutePath().toString();
                int indexTemp = filePathStr.indexOf(storageId + "/" + repositoryId);
                String temp = filePathStr.
                        substring(indexTemp + (storageId + "/" + repositoryId).length(), filePathStr.length());
                if (temp.startsWith("/")) {
                    temp = temp.substring(1, temp.length());
                }
                list.add(temp);
                // 添加跨节点元数据
                RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), temp);
                metaData.put(temp, getMetaData(srcRepositoryPath));
            }
            // 获取docker version file path
            if (isDockerVersionPath) {
                // copy blobs manifest
                String tempStr = storageId + File.separator + repositoryId + File.separator;
                int fPathIndex = absolutePath.lastIndexOf(tempStr);
                String relativizePath = absolutePath.substring(fPathIndex, absolutePath.length()).replace(tempStr, "");
                String[] arrayPath = relativizePath.split(File.separator);
                List<S3Path> fileContents = s3FilesPaths.stream().filter(file -> !(file.toAbsolutePath().endsWith(".sha256"))).collect(Collectors.toList());  //+propertiesBooter.getStorageBooterBasedir()+"/"+propertiesBooter.getVaultDirectory() + "/storages/"
                S3Path filePath = fileContents.get(0);
                String manifestString = Files.readString(filePath);
                ImageManifest manifest = JSON.parseObject(manifestString, ImageManifest.class);
                List<String> layerList = manifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
                String manifestConfig = manifest.getConfig().getDigest();
                layerList.add(manifestConfig);
                for (String layer : layerList) {
                    String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                    String blobSha256 = blob + ".sha256";
                    list.add(blob);
                    list.add(blobSha256);
                }
                String[] array = filePath.getKey().split(File.separator);
                String mainfestFile = arrayPath[0] + File.separator + "manifest" + File.separator + array[array.length - 1];
                String mainfestFileSha256 = mainfestFile + ".sha256";
                list.add(mainfestFile);
                list.add(mainfestFileSha256);
            }

        } else {
            List<File> files = RepositoryPathUtil.getNFSFiles(absolutePath);
            for (File file : files) {
                String fileAbsolutePath = file.getAbsolutePath();
                int indexTemp = fileAbsolutePath.indexOf(storageId + "/" + repositoryId);
                String temp = fileAbsolutePath.
                        substring(indexTemp + (storageId + "/" + repositoryId).length(), fileAbsolutePath.length());
                if (temp.startsWith("/")) {
                    temp = temp.substring(1, temp.length());
                }
                list.add(temp);
                // 添加跨节点元数据
                RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), temp);
                metaData.put(temp, getMetaData(srcRepositoryPath));
            }
            // 获取docker version file path
            if (isDockerVersionPath) {
                // 添加 blobs maninfest file path
                String tempStr = storageId + File.separator + repositoryId + File.separator;
                int fPathIndex = absolutePath.lastIndexOf(tempStr);
                String relativizePath = absolutePath.substring(fPathIndex, absolutePath.length()).replace(tempStr, "");
                String[] arrayPath = relativizePath.split(File.separator);
                List<File> fileContents = files.stream().filter(f -> !(f.getName().endsWith(".sha256"))).collect(Collectors.toList());
                File fileBolb = fileContents.get(0);
                String manifestString = Files.readString(fileBolb.toPath());
//                String manifestString = FileUtil.readString(repositoryPath.toAbsolutePath() + File.separator + fileBolb.getName(), "UTF-8");
                ImageManifest manifest = JSON.parseObject(manifestString, ImageManifest.class);
                List<String> layerList = manifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
                String manifestConfig = manifest.getConfig().getDigest();
                layerList.add(manifestConfig);
                for (String layer : layerList) {
                    String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                    String blobSha256 = blob + ".sha256";
                    list.add(blob);
                    list.add(blobSha256);
                }
                String mainfestFile = arrayPath[0] + File.separator + "manifest" + File.separator + fileBolb.getName();
                String mainfestFileSha256 = mainfestFile + ".sha256";
                list.add(mainfestFile);
                list.add(mainfestFileSha256);
            }
        }
        return new PromotionFileRelativePath(list, metaData);
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
        try {
            FormDataMultiPart part = new FormDataMultiPart();
            part.field("storageId", uploadDto.getStorageId());
            part.field("repostoryId", uploadDto.getRepostoryId());

            HashMap<String, String> filePathMap = new HashMap<String, String>();
            uploadDto.getPathMap().forEach((x, y) -> {
                y.forEach((j, z) -> {
                    part.bodyPart(new StreamDataBodyPart("files", z, j));
                    filePathMap.put(j, x);
                });
            });
            part.field("filePathMap", JSON.toJSONString(filePathMap));
            part.field("fileMetaDataMap", JSON.toJSONString(uploadDto.getFileMetaDataMap()));
            Client client = clientPool.getRestClient();
            WebTarget resource = client.register(MultiPartWriter.class).target(url);
            Response response = resource.request(MediaType.APPLICATION_JSON).header("Mime-Version", "1.0").
                    post(Entity.entity(part, Boundary.addBoundary(MediaType.MULTIPART_FORM_DATA_TYPE)));
            if (response.getStatus() != 200) {
                throw new Exception("upload failed ");
            }
        } catch (Exception e) {
            throw new Exception(e.getMessage());
        } finally {
            uploadDto.getPathMap().forEach((x, y) -> {
                y.forEach((j, z) -> {
                    if (null != z) {
                        try {
                            z.close();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }

                    }
                });
            });
        }
        return "上传成功";
    }

    public String getMetaData(RepositoryPath srcPath) {
        String rs = "";
        try {
            Artifact artifact = artifactWebService.getArtifact(srcPath);
            if (Objects.isNull(artifact)) {
                return rs;
            }
            rs = artifact.getMetadata();
        } catch (Exception e) {
            log.error("Exception {}", e.getMessage());
        }
        return rs;
    }

    public void setMetaData(RepositoryPath repositoryPath, String metadata) {
        try {
            if (StringUtils.isBlank(metadata)) {
                return;
            }
            Artifact artifact = artifactWebService.getArtifact(repositoryPath);
            if (Objects.isNull(artifact)) {
                throw new RuntimeException("artifact is null");
            }
            artifact.setMetadata(metadata);
            artifactService.saveOrUpdateArtifact(artifact);
        } catch (Exception e) {
            log.error("Exception {} {}", e.getMessage(), repositoryPath.toString());
        }

    }

}
