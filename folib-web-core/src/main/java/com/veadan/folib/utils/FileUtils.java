package com.veadan.folib.utils;

import com.veadan.folib.services.ArtifactManagementService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.system.ApplicationHome;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;

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
    final String TEMP_DIR = "/temp";

    /**
     * jar所在的目录
     *
     * @return
     */
    public String getJarHomePath() {
        ApplicationHome h = new ApplicationHome(getClass());
        File jarF = h.getSource();
        return jarF.getParentFile().toString();
    }


    /**
     * 写入文件
     *
     * @param target
     * @param src
     * @throws IOException
     */
    public static void write(String target, InputStream src) throws IOException {
        OutputStream os = new FileOutputStream(target);
        byte[] buf = new byte[1024];
        int len;
        while (-1 != (len = src.read(buf))) {
            os.write(buf, 0, len);
        }
        os.flush();
        os.close();
    }

    /**
     * 分块写入文件
     *
     * @param target
     * @param targetSize
     * @param src
     * @param srcSize
     * @param chunks
     * @param chunk
     * @throws IOException
     */
    public static void writeWithBlok(String target, Long targetSize, InputStream src, Long srcSize, Integer chunks, Integer chunk) throws IOException {
        RandomAccessFile randomAccessFile = new RandomAccessFile(target, "rw");
        randomAccessFile.setLength(targetSize);
        if (chunk == chunks - 1) {
            randomAccessFile.seek(targetSize - srcSize);
        } else {
            randomAccessFile.seek(chunk * srcSize);
        }
        byte[] buf = new byte[1024];
        int len;
        while (-1 != (len = src.read(buf))) {
            randomAccessFile.write(buf, 0, len);
        }
        randomAccessFile.close();
    }

    /**
     * 上传文件
     *
     * @param fileDir
     * @param fileName
     * @param chunk
     * @param bytes
     */
    public void upload(String fileDir, String fileName, Integer chunk, byte[] bytes) {

        RandomAccessFile tempRaf = null;
        FileChannel fileChannel = null;
        MappedByteBuffer mappedByteBuffer = null;
        try {
            String uploadDirPath = getJarHomePath();
            File tmpFile = createTmpFile(fileDir, fileName);
            tempRaf = new RandomAccessFile(tmpFile, "rw");
            fileChannel = tempRaf.getChannel();

            long chunkSize = 0;
            //写入该分片数据
            long offset = 0;
            byte[] fileData = bytes;
            logger.info("------------------>:fileName:{} file.szie:{}",fileName,bytes.length);
            mappedByteBuffer = fileChannel
                    .map(FileChannel.MapMode.READ_WRITE, offset, fileData.length);
            mappedByteBuffer.put(fileData);
        } catch (FileNotFoundException e) {
            logger.error(e.getMessage(), e);
        } catch (IOException e) {
            logger.error(e.getMessage(), e);
        } finally {
            try {
                fileChannel.close();
                tempRaf.close();
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
                .append(getJarHomePath())
                .append(TEMP_DIR).append("/")
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
                .append(getJarHomePath())
                .append(TEMP_DIR).append("/")
                .append(fileDir).append("/")
                .append(fileName).toString();

        Path path = Paths.get(dir);
        try {
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
        } catch (IOException ex) {
            logger.error(ex.getMessage(), ex);
        }
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
                    .append(getJarHomePath())
                    .append(TEMP_DIR).append("/")
                    .append(fileDir).append("/")
                    .append(fileName).toString();

            inputStream = new FileInputStream(filePath);
        } catch (FileNotFoundException e) {
            logger.error(e.getMessage(), e);
        } finally {
            return inputStream;
        }
    }

    public Long getFileSize(String fileDir, String fileName) {
        FileChannel to = null;
        long offset = 0L;
        try {
            String filePath = new StringBuffer()
                    .append(getJarHomePath())
                    .append(TEMP_DIR).append("/")
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
                .append(getJarHomePath())
                .append(TEMP_DIR).append("/")
                .append(fileDir).append("/")
                .append(fileName).toString();
        FileChannel to = null;
        try {
             to = new FileOutputStream(filePath).getChannel();

            offset = to.position();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
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

}
