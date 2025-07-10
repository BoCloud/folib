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
package com.folib.enums;

import com.github.luben.zstd.ZstdInputStream;
import lombok.Getter;
import org.apache.commons.compress.compressors.xz.XZCompressorInputStream;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;

/**
 * @author veadan
 * @since 2024-09-03 10:53
 */
@Getter
public enum ArchiveFormat {

    GZ(".gz") {
        public InputStream unzipControl(ByteArrayInputStream controlArchiveStream) throws IOException {
            return new GZIPInputStream(controlArchiveStream);
        }
    },
    XZ(".xz") {
        public InputStream unzipControl(ByteArrayInputStream controlArchiveStream) throws IOException {
            return new XZCompressorInputStream(controlArchiveStream);
        }
    },
    ZX(".zst"){
        public InputStream unzipControl(ByteArrayInputStream controlArchiveStream) throws IOException {
            return new ZstdInputStream(controlArchiveStream);
        }

    },
    NONE("") {
        public InputStream unzipControl(ByteArrayInputStream controlArchiveStream) throws IOException {
            return controlArchiveStream;
        }
    };

    private String extension;

     ArchiveFormat(String extension) {
        this.extension = extension;
    }

    public abstract InputStream unzipControl(ByteArrayInputStream controlArchiveStream) throws IOException;

}
