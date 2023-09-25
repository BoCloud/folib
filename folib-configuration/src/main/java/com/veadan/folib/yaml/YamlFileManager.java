package com.veadan.folib.yaml;

import cn.hutool.extra.spring.SpringUtil;
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;
import com.veadan.folib.util.ServiceLoaderUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.GenericTypeResolver;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import javax.annotation.PreDestroy;
import java.io.*;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Objects;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author veadan
 * @author Veadan
 * @author cbono
 */
public abstract class YamlFileManager<T> {
    private final Logger logger = LoggerFactory.getLogger(getClass());

    private final Class<T> myClazz;

    private final YAMLMapper yamlMapper;

    private final ThreadPoolTaskExecutor threadPool;

    protected YamlFileManager(YAMLMapperFactory yamlMapperFactory) {
        this(yamlMapperFactory, new Class[0]);
    }

    @SuppressWarnings("unchecked")
    protected YamlFileManager(YAMLMapperFactory yamlMapperFactory, Class<?>... classes) {
        myClazz = (Class<T>) GenericTypeResolver.resolveTypeArgument(getClass(), YamlFileManager.class);
        yamlMapper = yamlMapperFactory.create(ServiceLoaderUtils.load(classes));
        threadPool = buildThreadPoolTaskExecutor(1, 1);
    }

    protected abstract Resource getResource();

    public synchronized void store(final T configuration) throws IOException {
        Objects.nonNull(configuration);

        Resource resource = getResource();
        //Backup
        String backupPath = getBackupPath(), fileName = "write_before_" + resource.getFilename();
        backup(Files.readAllBytes(resource.getFile().toPath()), fileName, backupPath);

        //Check that target resource stored on FS and not under classpath for example
        if (!resource.isFile() || resource instanceof ClassPathResource) {
            logger.warn("Skip resource store [{}]", resource);
            return;
        }

        // Write the content - we know its a file at this point - use resource.getFile to work w/ Windows
        try (OutputStream os = new BufferedOutputStream(Files.newOutputStream(resource.getFile().toPath()))) {
            yamlMapper.writeValue(os, configuration);
        }
        fileName = "write_after_" + resource.getFilename();
        backup(Files.readAllBytes(resource.getFile().toPath()), fileName, backupPath);
        deleteOldBackups(Paths.get(backupPath));
    }

    public synchronized T read() throws IOException {
        Resource resource = getResource();
        if (!resource.exists()) {
            return null;
        }
        //backup
        String backupPath = getBackupPath(), fileName = "read_" + resource.getFilename();
        backup(Files.readAllBytes(resource.getFile().toPath()), fileName, backupPath);
        // Read the content - use FileInputStream for file resources to work w/ Windows
        try (InputStream inputStream = new BufferedInputStream(resource.isFile() ?
                new FileInputStream(resource.getFile()) :
                resource.getInputStream())) {
            return yamlMapper.readValue(inputStream, myClazz);
        }
    }

    public void backup(byte[] bytes, String fileName, String backupPath) {
        Runnable runnable = () -> {
            try {
                LocalDateTime localDateTime = LocalDateTime.now();
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
                String currentDate = localDateTime.format(formatter);
                Path backupDir = Files.createDirectories(Paths.get(backupPath, currentDate));
                formatter = DateTimeFormatter.ofPattern("HH_mm");
                String currentTime = localDateTime.format(formatter);
                // Create a backup file path
                Path backupFilePath = backupDir.resolve(currentTime + "_" + fileName);
                // Copy the current configuration to the backup file
                Files.write(backupFilePath, bytes);
                logger.debug("备份 [{}] 成功", backupFilePath.toString());
            } catch (Exception ex) {
                try {
                    logger.error("备份文件失败: [{}] [{}]", fileName, ExceptionUtils.getStackTrace(ex));
                } catch (Exception e) {
                    logger.error("备份文件失败: [{}]", ExceptionUtils.getStackTrace(ex));
                }
            }
        };
        threadPool.submit(runnable);
    }

    public void deleteOldBackups(Path backupDir) {
        Runnable runnable = () -> {
            LocalDateTime localDateTime = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            String directoryName;
            try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(backupDir)) {
                for (Path backupFile : directoryStream) {
                    try {
                        if (Files.isHidden(backupFile)) {
                            continue;
                        }
                        if (Files.isDirectory(backupFile)) {
                            directoryName = backupFile.getFileName().toString();
                            LocalDate backupDate = LocalDate.parse(directoryName, formatter);
                            long daysDifference = ChronoUnit.DAYS.between(backupDate, localDateTime);
                            if (daysDifference >= 2) {
                                deleteBackupDirectory(backupFile);
                            }
                        }
                    } catch (Exception ex) {
                        logger.error("删除备份文件失败: [{}] [{}]", backupFile.toString(), ExceptionUtils.getStackTrace(ex));
                    }
                }
            } catch (Exception ex) {
                logger.error("删除备份文件失败: [{}]", ExceptionUtils.getStackTrace(ex));
            }
        };
        threadPool.submit(runnable);
    }

    private void deleteBackupDirectory(Path directoryPath) throws IOException {
        FileUtils.deleteDirectory(directoryPath.toFile());
        logger.debug("删除备份目录 [{}] 成功", directoryPath.toString());
    }

    private String getBackupPath() {
        String home = System.getProperty("folib.vault");
        if (StringUtils.isBlank(home)) {
            home = SpringUtil.getProperty("folib.vault");
        }
        logger.debug("备份目录: [{}]", home);
        return StringUtils.removeEnd(home, File.separator) + File.separator + "backup";
    }

    @PreDestroy
    public void destroy() {
        // 关闭线程池
        threadPool.shutdown();
    }

    private ThreadPoolTaskExecutor buildThreadPoolTaskExecutor(Integer corePoolSize, Integer maxPoolSize) {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(corePoolSize);
        executor.setMaxPoolSize(maxPoolSize);
        executor.setQueueCapacity(Integer.MAX_VALUE);
        executor.setKeepAliveSeconds(60);
        executor.setThreadNamePrefix("backup_");
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(60);
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        executor.initialize();
        return executor;
    }
}
