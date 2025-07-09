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
package com.folib.extractor;


import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import javax.annotation.Nonnull;

import com.folib.model.CargoMetadata;
import com.folib.providers.io.RepositoryPath;
import com.folib.utils.CargoUtil;
import lombok.Generated;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CargoMetadataExtractor {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(CargoMetadataExtractor.class);

    /**
     * 从给定的仓库路径中提取cargo元数据
     * 此方法负责解析和提取与特定仓库路径关联的cargo元数据它可能涉及到解析路径结构，
     * 读取元数据文件，或者解码存储在该路径下的元数据信息具体实现细节取决于仓库系统的设计
     * 和存储元数据的方式
     *
     * @param repositoryPath 仓库路径，表示需要提取元数据的cargo位置这可以用来定位仓库中特定的cargo或元数据文件
     * @return CargoMetadata 返回提取的cargo元数据对象，该对象包含关于cargo的信息，如名称、版本、依赖等
     */
    @Nonnull
    public CargoMetadata extract(RepositoryPath repositoryPath) {
        String path = repositoryPath.getPath();
        if(!Files.exists(repositoryPath)){
            return null;
        }
        try (InputStream in =  new BufferedInputStream(Files.newInputStream(repositoryPath));
             InputStream uncompressedStream = new BufferedInputStream( new GzipCompressorInputStream(in));
             ArchiveInputStream stream = (new ArchiveStreamFactory()).createArchiveInputStream(uncompressedStream);) {
            log.debug("Reading contents of cargo crate '{}' with size '{}'", path, Files.size(repositoryPath));
            CargoMetadata cargoMetadata = extractCargoMetadata(path, stream);
            stream.close();
            uncompressedStream.close();
            in.close();
            return cargoMetadata;
        } catch (Exception e) {
            log.error("Error while extracting metadata from {}: {}", repositoryPath.getFileName(), e.getMessage());
            log.debug("Error while extracting metadata from chart: " + e.getMessage(), e);
            e.printStackTrace();
            return new CargoMetadata();
        }
    }

    /**
     * 从给定的路径和输入流中提取cargo元数据
     * 此方法用于解析和提取存放在特定路径下的cargo元数据信息，使用提供的输入流进行读取操作
     * 主要用于处理cargo信息的归档文件，从中提取出必要的元数据信息，如cargo名称、重量、目的地等
     *
     * @param path 归档文件的路径，用于记录和追踪处理中的文件位置
     * @param stream 输入流，用于读取归档文件中的内容，必须实现 ArchiveInputStream 接口以支持归档文件的读取
     * @return CargoMetadata 返回提取出的cargo元数据对象，包含解析后的cargo信息
     * @throws IOException 如果在读取过程中发生I/O错误，将抛出此异常
     */
    private CargoMetadata extractCargoMetadata(String path, ArchiveInputStream stream) throws IOException {
        CargoMetadata cargoMetadata = null;
        ArchiveEntry entry;
        while ((entry = stream.getNextEntry()) != null) {
            if (stream.canReadEntryData(entry)) {
                cargoMetadata = extractFileMetadata(stream, entry);
                if (cargoMetadata != null)
                    break;
                continue;
            }
            log.warn("Encountered unreadable archive entry '{}' inside archive {}", entry.getName(), path);
        }
        if (cargoMetadata == null) {
            cargoMetadata = new CargoMetadata();
            log.error("Could not extract metadata from crate {}", path);
        }
        return cargoMetadata;
    }

    /**
     * 从档案输入流中提取文件的元数据
     * 此方法用于解析和提取给定档案条目的元数据信息，元数据包括文件的路径、大小和其他相关属性
     * 它主要用于处理应用程序中的文件解压缩和分析操作
     *
     * @param stream 档案输入流，用于读取档案文件中的数据
     * @param entry 档案条目，表示当前要提取元数据的文件或目录
     * @return 返回一个CargoMetadata对象，该对象包含提取的文件元数据
     * @throws IOException 如果在读取档案输入流或处理档案条目时发生I/O错误
     */
    private CargoMetadata extractFileMetadata(ArchiveInputStream stream, ArchiveEntry entry) throws IOException {
        File entryFile = new File(entry.getName());
        CargoMetadata cargoMetadata = null;
        if (!entry.isDirectory() && "Cargo.toml".equals(entryFile.getName())) {
            log.debug("Found Cargo.toml file: {}", entry.getName());
            String body = IOUtils.toString((InputStream) stream, Charset.defaultCharset());
            cargoMetadata = CargoUtil.parseCargoConfig(body);
        }
        return cargoMetadata;
    }
}
