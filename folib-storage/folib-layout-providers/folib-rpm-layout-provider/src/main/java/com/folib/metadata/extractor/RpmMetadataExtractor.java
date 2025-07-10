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
package com.folib.metadata.extractor;


import org.redline_rpm.header.Format;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class RpmMetadataExtractor  {
    public RpmMetadata extract(Path repositoryPath) throws Exception {
        if (repositoryPath == null) {
            return null;
        } else {
            Format rpmFormat = RpmFormatReader.read(repositoryPath.toString());
            if (rpmFormat == null) {
                return null;
            } else {
                RpmMetadata metadata = new RpmFormatInterpreter().interpret(rpmFormat);
                Path shaPath = Path.of(repositoryPath.toString());
                metadata.setSha1Digest(readSHA1FileContent(shaPath));
                metadata.setArtifactRelativePath(repositoryPath.getFileName().toString());
                metadata.setLastModified(getlastModified(repositoryPath));
                metadata.setSize(getFileSize(repositoryPath));
                return metadata;
            }
        }
    }

    public  String readSHA1FileContent(Path filePath) throws Exception {
        try {
            //return getSHA1(filePath);
            return Files.readString(Paths.get(filePath.toString()+".sha1"));
        } catch (IOException e) {
            throw e;
        }
    }

    public long getFileSize(Path filePath) throws IOException {
        return Files.size(filePath);
    }

    public long getlastModified(Path filePath) throws IOException {
        return Files.getLastModifiedTime(filePath).toMillis();
    }

    public  String getSHA1(Path filePath) throws NoSuchAlgorithmException, IOException {
        MessageDigest digest = MessageDigest.getInstance("SHA-1");
        try (var in = Files.newInputStream(filePath)) {
            byte[] buffer = new byte[4096];
            int read;
            while ((read = in.read(buffer)) > 0) {
                digest.update(buffer, 0, read);
            }
        }
        byte[] hash = digest.digest();
        return bytesToHex(hash);
    }

    private  String bytesToHex(byte[] hash) {
        StringBuilder hexString = new StringBuilder(2 * hash.length);
        for (int i = 0; i < hash.length; i++) {
            String hex = Integer.toHexString(0xff & hash[i]);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }
}