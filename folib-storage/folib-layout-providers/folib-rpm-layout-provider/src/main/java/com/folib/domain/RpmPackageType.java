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
package com.folib.domain;

import org.tukaani.xz.simple.ARM;
import org.tukaani.xz.simple.IA64;
import org.tukaani.xz.simple.SPARC;

public enum RpmPackageType
{
    SOURCE("src"),
    BINARY(""),
    NOARCH("noarch"),
    I386("i386"),
    ALPHA("alpha"),
    SPARC("SPARC"),
    MIPS("mips"),
    PPC("ppc"),
    M68K("m68k"),
    IP("ip"),
    RS6000("rs6000"),
    IA64("ia64"),
    SPARC64("sparc64"),
    MIPSEL("mipsel"),
    ARM("arm"),
    MK68KMINT("mk68kmint"),
    S390("s390"),
    S390X("s390x"),
    PPC64("ppc64"),
    SH("sh"),
    XTENSA("xtensa"),
    X86_64("x86_64"),
    PPC64LE("ppc64le"),
    AARCH64("aarch64"),
    I686("i686"),
    MIPS64EL("mips64el"),
    SW_64("sw_64"),
    LOONGARCH64("loongarch64"),
    RISCV64("riscv64"),
    SRPMS("src"),
    SOURCES("sources"),
    UNKNOWN("unknown");

    private String postfix;

    public String getPostfix()
    {
        return postfix;
    }

    RpmPackageType(String postfix)
    {
        this.postfix = postfix;
    }







}
