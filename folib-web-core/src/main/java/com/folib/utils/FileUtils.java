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
package com.folib.utils;

import cn.hutool.core.io.FileUtil;
import cn.hutool.extra.spring.SpringUtil;
import com.folib.providers.io.RepositoryFiles;
import com.folib.scanner.common.exception.BusinessException;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * @ProjectName: folib-server
 * @Package: com.folib.utils
 * @ClassName: FileUtils
 * @Author: mac
 * @Description:
 * @Date: 2022/5/18 10:28
 * @Version: 1.0
 */
public class FileUtils {


    private static final Logger logger = LoggerFactory.getLogger(FileUtils.class);

    /**
     * 临时目录
     *
     * @return
     */
    public static String getTempPath() {
        return SpringUtil.getProperty("folib.temp");
    }

    /**
     * @return basePath
     */
    public static String getBasePath() {
        return getTempPath() + "/";
    }

    /**
     * 上传文件
     *
     * @param fileDir
     * @param fileName
     * @param bytes
     */
    public void upload(String fileDir, String fileName, byte[] bytes) {
        RandomAccessFile tempRaf = null;
        FileChannel fileChannel = null;
        MappedByteBuffer mappedByteBuffer = null;
        try {
            File tmpFile = createTmpFile(fileDir, fileName);
            tempRaf = new RandomAccessFile(tmpFile, "rw");
            fileChannel = tempRaf.getChannel();
            //写入该分片数据
            long offset = 0;
            logger.info("------------------>:filePath:{} fileName:{} fileSize:{}", tmpFile.getAbsolutePath(), fileName, bytes.length);
            mappedByteBuffer = fileChannel
                    .map(FileChannel.MapMode.READ_WRITE, offset, bytes.length);
            mappedByteBuffer.put(bytes);
        } catch (IOException e) {
            logger.error(e.getMessage(), e);
        } finally {
            try {
                if (Objects.nonNull(fileChannel)) {
                    fileChannel.close();
                }
                if (Objects.nonNull(tempRaf)) {
                    tempRaf.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

    }

    /**
     * 创建文件
     *
     * @param fileDir
     * @param fileName
     * @return
     */
    protected File createTmpFile(String fileDir, String fileName) {
        String dir = new StringBuffer()
                .append(getTempPath())
                .append("/")
                .append(fileDir).toString();
        File tmpDir = new File(dir);
        File tmpFile = new File(dir, fileName);
        if (!tmpDir.exists()) {
            tmpDir.mkdirs();
        }
        return tmpFile;
    }

    /**
     * 删除目录文件
     *
     * @param fileDir
     * @param fileName
     */
    public void deleteDir(String fileDir, String fileName) {
        String dir = new StringBuffer()
                .append(getTempPath())
                .append("/")
                .append(fileDir).append("/")
                .append(fileName).toString();

        Path path = Paths.get(dir);
        try {
            deletePath(path);
            Path parentPath = path.getParent();
            if (Files.isDirectory(parentPath) && RepositoryFiles.isDirectoryEmpty(parentPath)) {
                Files.deleteIfExists(parentPath);
            }
        } catch (IOException ex) {
            logger.error(ex.getMessage(), ex);
        }
    }

    /**
     * 删除文件或文件夹
     *
     * @param path 路径
     * @throws IOException io异常
     */
    public void deletePath(Path path) throws IOException {
        Files.walkFileTree(path, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                    throws IOException {
                try {
                    Files.delete(file);
                } catch (IOException e) {
                    logger.error("Failed to delete file: " + file + " - " + e.getMessage());
                }
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc)
                    throws IOException {
                try {
                    Files.delete(dir);
                } catch (IOException e) {
                    logger.error("Failed to delete directory: " + dir + " - " + e.getMessage());
                }
                return FileVisitResult.CONTINUE;
            }
        });
    }

    /**
     * 读取文件
     *
     * @param fileDir
     * @param fileName
     * @return
     */
    public FileInputStream getFile(String fileDir, String fileName) {
        FileInputStream inputStream = null;
        try {
            String filePath = new StringBuffer()
                    .append(getTempPath())
                    .append("/")
                    .append(fileDir).append("/")
                    .append(fileName).toString();

            inputStream = new FileInputStream(filePath);
        } catch (FileNotFoundException e) {
            logger.error(e.getMessage(), e);
            return null;
        }
        return inputStream;
    }

    /**
     * 读取文件
     *
     * @param fileDir
     * @param fileName
     * @return
     */
    public Path getPath(String fileDir, String fileName) {
        String filePath = getTempPath() +
                "/" +
                fileDir + "/" +
                fileName;
        return Path.of(filePath);
    }

    public Long getFileSize(String fileDir, String fileName) {
        FileChannel to = null;
        long offset = 0L;
        try {
            String filePath = new StringBuffer()
                    .append(getTempPath())
                    .append("/")
                    .append(fileDir).append("/")
                    .append(fileName).toString();

            to = new FileOutputStream(filePath).getChannel();
            offset = to.position();
        } catch (FileNotFoundException e) {
            logger.error(e.getMessage(), e);
        } finally {
            if (Objects.nonNull(to)) {
                try {
                    to.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return offset;
        }
    }

    public Long getOffset(String fileDir, String fileName) {
        long offset = 0L;
        String filePath = new StringBuffer()
                .append(getTempPath())
                .append("/")
                .append(fileDir).append("/")
                .append(fileName).toString();
        FileChannel to = null;
        try {
            to = new FileOutputStream(filePath).getChannel();

            offset = to.position();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (Objects.nonNull(to)) {
                try {
                    to.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return offset;
        }
    }

    public static String formatSize(long size) {
        if (size <= 0) {
            return "0 B";
        }
        String[] units = {"B", "KB", "MB", "GB", "TB"};
        int digitGroups = (int) (Math.log10(size) / Math.log10(1000));
        return String.format("%.1f %s", size / Math.pow(1000, digitGroups), units[digitGroups]);
    }

    /**
     * 文件切割
     * @param sourceFilePath 
     * @param destinationFolderPath
     * @param chunkSize 切割大小（KB） 
     * @throws IOException
     */
    public static List<String> splitFile(String sourceFilePath, String destinationFolderPath, long chunkSize) throws IOException {
        final List<String> sliceFilePathList = new ArrayList<>();
        
        File destinationFolder = new File(destinationFolderPath);
        if (!destinationFolder.exists()) {
            destinationFolder.mkdirs();
        }
        if (destinationFolderPath.endsWith("/") || destinationFolderPath.endsWith("\\")) {
            destinationFolderPath = destinationFolderPath.substring(0, destinationFolderPath.length() - 1);
        }

        try (RandomAccessFile sourceFile = new RandomAccessFile(sourceFilePath, "r");
             FileChannel sourceChannel = sourceFile.getChannel()) {
            final String name = new File(sourceFilePath).getName();
            long fileSize = sourceFile.length();
            long numberOfChunks = (long) Math.ceil((double) fileSize / chunkSize);

            for (long i = 0; i < numberOfChunks; i++) {
                long offset = i * chunkSize;
                int bufferSize = (int) Math.min(chunkSize, fileSize - offset);

                sourceChannel.position(offset);

                ByteBuffer buffer = ByteBuffer.allocate(bufferSize);
                sourceChannel.read(buffer);
                buffer.flip();

                String chunkFileName = String.format("%s%s%s-chunk%s", destinationFolderPath, File.separator, name, i);
                sliceFilePathList.add(chunkFileName);
                try (FileOutputStream outputStream = new FileOutputStream(chunkFileName)) {
                    outputStream.getChannel().write(buffer);
                }
            }
        }
        
        return sliceFilePathList;
    }

    /**
     * 文件合并
     * @param targetFilePath
     * @param sourceFilePaths 需要合并路径的集合
     * @return
     */
    public static boolean mergeFiles(String targetFilePath, List<String> sourceFilePaths)  {
        final File targetFile = new File(targetFilePath);
        if (!FileUtil.exist(targetFile)) {
            FileUtil.touch(targetFile);
        }
        
        try (BufferedOutputStream outputStream = new BufferedOutputStream(Files.newOutputStream(Paths.get(targetFilePath)))) {
            for (String sourceFilePath : sourceFilePaths) {
                try (BufferedInputStream inputStream = new BufferedInputStream(Files.newInputStream(Paths.get(sourceFilePath)))) {
                    byte[] buffer = new byte[1024];
                    int bytesRead;

                    while ((bytesRead = inputStream.read(buffer)) != -1) {
                        outputStream.write(buffer, 0, bytesRead);
                    }
                }
            }
        } catch (Exception e) {
            logger.error("文件合并失败", e);
            return false;
        }

        return true;
    }

    public static String getMD5(String filePath) {
        return getMD5(new File(filePath));
    }
    
    public static String getMD5(File file) {
        try {
            final FileInputStream fis = new FileInputStream(file);
            return getMD5(fis);
        } catch (Exception e) {
            logger.info("获取文件的MD5失败", e);
            return null;
        }
    }
    public static String getMD5(InputStream fileInputStream) {
        try {
            final MessageDigest md = MessageDigest.getInstance("MD5");
            final byte[] buffer = new byte[8192];
            int length;
            while ((length = fileInputStream.read(buffer)) != -1) {
                md.update(buffer, 0, length);
            }
            fileInputStream.close();
            final byte[] digest = md.digest();
            final BigInteger bigInt = new BigInteger(1, digest);
            return bigInt.toString(16);
        } catch (Exception e) {
            logger.info("获取文件的MD5失败", e);
            return null;
        }
    }


    public static boolean lockFileStatus(String filePath) {
        return lockFileNumber(filePath) > 0;
    }
    
    public synchronized static Integer lockFile(String filePath) {
        final File folderLockFile = getLockFile(filePath);
        if (!FileUtil.exist(folderLockFile)) {
            FileUtil.writeUtf8String(String.valueOf(0), folderLockFile);
        }

        Integer lockNumber = Optional.ofNullable(FileUtil.readUtf8String(folderLockFile)).map(Integer::parseInt).orElse(0);
        FileUtil.writeUtf8String(String.valueOf(++lockNumber), folderLockFile);

        return lockNumber;
    }

    public synchronized static Integer unlockFile(String filePath) {
        final File folderLockFile = getLockFile(filePath);
        Integer lockNumber = lockFileNumber(filePath);
        if (lockNumber > 0) {
            FileUtil.writeUtf8String(String.valueOf(--lockNumber), folderLockFile);
        }
        
        return lockNumber; 
    }

    public static Integer lockFileNumber(String filePath) {
        final File folderLockFile = getLockFile(filePath);
        return Optional.ofNullable(FileUtil.readUtf8String(folderLockFile)).map(Integer::parseInt).orElse(0);
    }
    
    private static File getLockFile(String filePath) {
        final File file = new File(filePath);
        
        if (!FileUtil.exist(file)) {
            throw new BusinessException("获取文件锁的文件或文件夹不存在");
        }

        if (FileUtil.isFile(file)) {
            return new File(String.format("%s.lock", filePath));
        } else {
            return new File(String.format("%s/folder.lock", StringUtils.chomp(filePath, "/")));
        }
    }

}
