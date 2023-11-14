package com.veadan.folib.utils;

import cn.hutool.extra.spring.SpringUtil;
import org.opencypher.v9_0.expressions.functions.E;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * @ProjectName: folib-server
 * @Package: com.veadan.folib.utils
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
            if (Files.isDirectory(parentPath) && Files.list(parentPath).count() <= 0) {
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
                Files.delete(file);
                return super.visitFile(file, attrs);
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc)
                    throws IOException {
                Files.delete(dir);
                return super.postVisitDirectory(dir, exc);
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
     * @since x.x.x
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
     * @since x.x.x
     */
    public static boolean mergeFiles(String targetFilePath, List<String> sourceFilePaths)  {
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
}
