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

public enum RpmPackageArch
{
    NOARCH("noarch"),
    I386("i386"),
    I686("i686"),
    X86_64("x86_64"),
    ALPHA("alpha"),
    SPARC("sparc"),
    MIPS("mips"),
    PCC("pcc"),
    PPC("ppc"),
    M68K("m68k"),
    SGI("sgi"),
    SOURCE("src"),
    BINARY(""),
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
    PPC64LE("ppc64le"),
    AARCH64("aarch64");;

    private String name;

    RpmPackageArch(String name)
    {
        this.name = name;
    }

    public String getName()
    {
        return name;
    }
}
