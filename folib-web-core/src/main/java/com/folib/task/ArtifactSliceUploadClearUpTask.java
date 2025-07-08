package com.folib.task;


import com.folib.components.DistributedLockComponent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.time.temporal.ChronoUnit;


@Slf4j
@Component
@EnableScheduling
public class ArtifactSliceUploadClearUpTask {

    @Value("${folib.temp}")
    private String tempPath;
    @Autowired
    private DistributedLockComponent distributedLockComponent;

    /**
     * 每天早上5点邮件发送漏洞信息
     * 0 0 5 * * ?
     */
    @Scheduled(cron = "0 0 5 * * ?")
    public void artifactSliceUploadClearUp() {
        String lockName = "ArtifactSliceUploadClearUpTask";
        long waitTime = 3L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                clearUp();
            } catch (Exception ex) {
                log.error("清理上传分片目录定时任务错误：{}", ExceptionUtils.getStackTrace(ex));
            } finally {
                distributedLockComponent.unLock(lockName, 3500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
        log.info("Scheduled artifactSliceUploadClearUp end");
    }


    public void clearUp() {
        // 要检查和删除的文件夹路径
        Path directoryPath = Paths.get( String.format("%s/artifactSliceUpload", tempPath));
        Instant oneDayAgo = Instant.now().minus(2, ChronoUnit.DAYS);

        try {
            // 使用 walkFileTree 来删除文件夹及其内容
            Files.walkFileTree(directoryPath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                    // 获取文件的创建时间
                    Instant creationTime = attrs.creationTime().toInstant();
                    if (creationTime.isBefore(oneDayAgo)) {
                        log.info("Deleting Artifact Slice Upload file: " + file.toString());
                        Files.delete(file);  // 删除文件
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                    BasicFileAttributes attrs = Files.readAttributes(dir, BasicFileAttributes.class);
                    Instant creationTime = attrs.creationTime().toInstant();
                    if (creationTime.isBefore(oneDayAgo)) {
                        log.info("Deleting Artifact Slice Upload directory: " + dir.toString());
                        Files.delete(dir);  // 删除目录
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (IOException e) {
            log.error("Deleting Artifact Slice Upload error:", e);
        }
    }
}
