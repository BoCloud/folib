package com.veadan.folib.util;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.cloud.storage.s3fs.S3FileSystem;
import com.veadan.folib.cloud.storage.s3fs.S3Iterator;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.providers.io.RepositoryPath;
import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

@Slf4j
public class RepositoryPathUtil {
    public static List<S3Path> getS3FiePaths(S3Path s3Path) throws Exception {
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
     * 获取绝对路径下的所有文件
     *
     * @param path path
     */
    public static List<File> getNFSFiles(String path) {
        int fileNum = 0, folderNum = 0;
        File file = new File(path);
        LinkedList<File> list = new LinkedList<>();
        List<File> resultList = new ArrayList<>();

        if (file.exists()) {
            if (null == file.listFiles()) {
                resultList.add(file);
                return resultList;
            }
            for (File f : file.listFiles()) {
                if (f.isDirectory()) {
                    list.add(f);
                    folderNum++;
                } else {
                    resultList.add(f);
                    fileNum++;
                }
            }
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
        log.info("path:{}，文件夹数量:{} ,文件数量:{}", path, folderNum, fileNum);
        return resultList;
    }

    public static List<String> getFileRelativePaths(RepositoryPath repositoryPath) throws Exception {
        String repositoryId = repositoryPath.getRepository().getId();
        String storageId = repositoryPath.getRepository().getStorage().getId();
        String absolutePath = repositoryPath.toAbsolutePath().toString();
        List<String> list = new ArrayList<String>();
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
            }
        }
        return list;
    }
}
