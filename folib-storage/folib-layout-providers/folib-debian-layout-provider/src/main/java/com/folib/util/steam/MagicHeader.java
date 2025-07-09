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
package com.folib.util.steam;

import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.ar.ArArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream;

/**
 * @author veadan
 * @since 2024-09-03 10:18
 */
public enum MagicHeader {

    TAR(new byte[]{117, 115, 116, 97, 114}, 257, TarArchiveInputStream.class),
    AR(new byte[]{33, 60, 97, 114, 99, 104, 62}, 0, ArArchiveInputStream.class),
    ZIP(new byte[]{80, 75}, 0, ZipArchiveInputStream.class);

    private byte[] header;
    private int offset;
    private Class<? extends ArchiveInputStream> archiveType;

    private MagicHeader(byte[] header, int offset, Class archiveType) {
        this.header = header;
        this.offset = offset;
        this.archiveType = archiveType;
    }

    public byte[] getHeader() {
        return this.header;
    }

    public int getOffset() {
        return this.offset;
    }

    public Class<? extends ArchiveInputStream> getArchiveType() {
        return this.archiveType;
    }
}
