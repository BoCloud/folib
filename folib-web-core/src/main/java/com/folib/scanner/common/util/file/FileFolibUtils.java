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
package com.folib.scanner.common.util.file;

import com.folib.scanner.common.exception.BusinessException;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.http.entity.ContentType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;

public class FileFolibUtils {

    /**
     * 获取文件的MD5
     * @param file
     * @return
     */
    public static String getMD5(MultipartFile file) {
        String md5Hex = null;
        try {
            md5Hex = DigestUtils.md5Hex(file.getInputStream());
        } catch (IOException e) {
            throw new BusinessException("文件获取MD5失败");
        }

        return md5Hex;
    }

    /**
     * 获取URL的文件并保存到指定的目录下
     * @param url 下载地址
     * @param filePath   文件夹目录
     * @param originalFilename  文件名称包含后缀
     * @param method  get/post
     * @return
     */
    public static File saveUrlAs(String url, String filePath, String originalFilename, String method) {
        //System.out.println("fileName---->"+filePath);
        //创建不同的文件夹目录
        File file = new File(filePath);
        //判断文件夹是否存在
        if (!file.exists()) {
            //如果文件夹不存在，则创建新的的文件夹
            file.mkdirs();
        }
        FileOutputStream fileOut = null;
        HttpURLConnection conn = null;
        InputStream inputStream = null;
        try {
            // 建立链接
            URL httpUrl = new URL(url);
            conn = (HttpURLConnection) httpUrl.openConnection();
            //以Post方式提交表单，默认get方式
            conn.setRequestMethod(method);
            conn.setDoInput(true);
            conn.setDoOutput(true);
            // post方式不能使用缓存
            conn.setUseCaches(false);
            //连接指定的资源
            conn.connect();
            //获取网络输入流
            inputStream = conn.getInputStream();
            BufferedInputStream bis = new BufferedInputStream(inputStream);
            //判断文件的保存路径后面是否以/结尾
            if (!filePath.endsWith(File.separator)) {

                filePath += File.separator;

            }
            //写入到文件（注意文件保存路径的后面一定要加上文件的名称）
            fileOut = new FileOutputStream(filePath + originalFilename);
            BufferedOutputStream bos = new BufferedOutputStream(fileOut);

            byte[] buf = new byte[4096];
            int length = bis.read(buf);
            //保存文件
            while (length != -1) {
                bos.write(buf, 0, length);
                length = bis.read(buf);
            }
            bos.close();
            bis.close();
            conn.disconnect();
        } catch (Exception e) {
            e.printStackTrace();
            throw new BusinessException("临时缓存文件出现异常,请查看后台日志");
        }
        return file;
    }

    /**
     *      * 递归删除文件或者目录
     *      * @param file_path
     *
     */
    public static void deleteEveryThing(String file_path) {
        try {
            File file = new File(file_path);
            if (!file.exists()) {
                return;
            }
            if (file.isFile()) {
                file.delete();
            } else {
                File[] files = file.listFiles();
                for (int i = 0; i < files.length; i++) {
                    String root = files[i].getAbsolutePath();//得到子文件或文件夹的绝对路径
                    deleteEveryThing(root);
                }
                file.delete();
            }
        } catch (Exception e) {
            System.out.println("删除文件失败");
        }
    }

    public static MultipartFile file2MultipartFile(File pdfFile) {

        try {
            FileInputStream fileInputStream = new FileInputStream(pdfFile);
            MultipartFile multipartFile = new MockMultipartFile(pdfFile.getName(), pdfFile.getName(),
                    ContentType.APPLICATION_OCTET_STREAM.toString(), fileInputStream);
            return multipartFile;
        } catch (IOException e) {
           throw new BusinessException("文件转换失败");
        }
    }

}
