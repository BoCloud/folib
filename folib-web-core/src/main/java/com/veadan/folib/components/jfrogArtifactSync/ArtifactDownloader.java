package com.veadan.folib.components.jfrogArtifactSync;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;
import org.jfrog.artifactory.client.RepositoryHandle;
import org.jfrog.artifactory.client.model.File;
import org.jfrog.artifactory.client.model.Folder;
import org.jfrog.artifactory.client.model.Item;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 * @author huayanjun
 * @since 2024-12-31 13:09
 */
@Slf4j
public class ArtifactDownloader {

    private final Artifactory artifactory;
    private final String downloadBasePath;
    private final int threadPoolSize;
    private final ExecutorService executorService;
    private final ObjectMapper objectMapper;

    public ArtifactDownloader(String url, String username, String apiKey,
                              String downloadPath, int threadPoolSize) {
        this.artifactory = ArtifactoryClientBuilder.create()
                .setUrl(url)
                .setUsername(username)
                .setPassword(apiKey)
                .build();
        this.downloadBasePath = downloadPath;
        this.threadPoolSize = threadPoolSize;
        this.executorService = Executors.newFixedThreadPool(threadPoolSize);
        this.objectMapper = new ObjectMapper();
    }

    public void downloadAllArtifacts(String repoName) {
        try {
            // 1. 获取所有制品
            List<String> artifacts = getAllArtifacts(repoName);
            log.info("仓库{} 发现制品{}", repoName, artifacts.size());
            // 2. 创建下载任务
            List<CompletableFuture<Void>> downloadTasks = new ArrayList<>();
            for (String artifact : artifacts) {
                CompletableFuture<Void> task = CompletableFuture.runAsync(
                        () -> downloadArtifactWithProperties(repoName, artifact),
                        executorService
                );
                downloadTasks.add(task);
            }
            // 3. 等待所有任务完成
            CompletableFuture.allOf(
                    downloadTasks.toArray(new CompletableFuture[0])
            ).get();
            log.info("All downloads completed!");
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            shutdown();
        }
    }

    public List<String> getAllArtifacts(String repoName) {
        List<String> artifacts = new ArrayList<>();
        RepositoryHandle repositoryHandle = artifactory.repository(repoName);
        try {
            // Start from the root path
            fetchArtifactsRecursively(repositoryHandle, "/", artifacts);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return artifacts;
    }

    private static void fetchArtifactsRecursively(RepositoryHandle repositoryHandle, String path, List<String> artifacts) {
        try {
            // Get the list of items in the current path
            Folder folder = repositoryHandle.folder(path).info();
            for (Item child : folder.getChildren()) {
                if (child.isFolder()) {
                    fetchArtifactsRecursively(repositoryHandle, child.getPath(), artifacts);
                } else {
                    File file = repositoryHandle.file(child.getPath()).info();
                    artifacts.add(file.getPath());

                }
            }

        } catch (Exception e) {
            System.err.println("Error fetching artifacts from path: " + path);
            e.printStackTrace();
        }
    }

    private void downloadArtifactWithProperties(String repoName, String artifactPath) {
        try {
            // 构建下载路径
            Path targetPath = Paths.get(downloadBasePath, repoName, artifactPath);
            Files.createDirectories(targetPath.getParent());

            // 1. 下载制品文件
            downloadArtifact(repoName, artifactPath, targetPath);

            // 2. 保存制品属性
            saveArtifactProperties(repoName, artifactPath, targetPath);

            System.out.printf("Successfully downloaded: %s with properties%n",
                    artifactPath);

        } catch (Exception e) {
            log.error("");
        }
    }

    private void downloadArtifact(String repoName, String artifactPath,
                                  Path targetPath) throws IOException {
        try (InputStream is = artifactory.repository(repoName)
                .download(artifactPath)
                .doDownload();
             OutputStream os = Files.newOutputStream(targetPath)) {

            byte[] buffer = new byte[8192];
            int bytesRead;
            while ((bytesRead = is.read(buffer)) != -1) {
                os.write(buffer, 0, bytesRead);
            }
        }
    }

    private void saveArtifactProperties(String repoName, String artifactPath,
                                        Path targetPath) throws IOException {
        // 获取制品属性
        Map<String, List<String>> properties = artifactory.repository(repoName)
                .file(artifactPath)
                .getProperties();

        // 保存属性到JSON文件
        Path propertiesPath = Paths.get(
                targetPath.toString() + ".properties.json"
        );
        objectMapper.writeValue(propertiesPath.toFile(), properties);
    }

    private void shutdown() {
        try {
            executorService.shutdown();
            if (!executorService.awaitTermination(1, TimeUnit.HOURS)) {
                executorService.shutdownNow();
            }
        } catch (InterruptedException e) {
            executorService.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    // 下载进度监控类
    @lombok.Data
    public static class DownloadStats {

    }


}
