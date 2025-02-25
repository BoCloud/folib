package com.veadan.folib.domain;

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
