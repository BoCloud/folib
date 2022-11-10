package com.veadan.folib.promotion;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.cloud.storage.s3fs.S3FileSystem;
import com.veadan.folib.cloud.storage.s3fs.S3Iterator;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.dto.TargetRepositoyDto;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
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

    public void handleCopy(String path, Repository destRepository, Repository srcRepository) throws Exception {
        int fileNum = 0, folderNum = 0;
        File file = new File(path);
        LinkedList<File> list = new LinkedList<>();

        if (file.exists()) {
            if (null == file.listFiles()) {
                return;
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
                        String fPath = f.getAbsolutePath().toString();
                        int fPathIndex = fPath.lastIndexOf(srcRepository.getId() + File.separator);
                        String temp = fPath.substring(fPathIndex, fPath.length()).replace(srcRepository.getId() + File.separator, "");
                        RepositoryPath destPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), temp);

                        try (InputStream is = Files.newInputStream(f.toPath());) {
                            artifactManagementService.store(destPath, is);
                        } catch (IOException e) {
                            e.printStackTrace();
                            throw new Exception(e.getMessage());
                        }
                        fileNum++;
                    }
                }
            }
        } else {
            log.info("文件不存在!");
        }
        log.info("文件夹数量:{} ,文件数量:{}", folderNum, fileNum);
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

}
