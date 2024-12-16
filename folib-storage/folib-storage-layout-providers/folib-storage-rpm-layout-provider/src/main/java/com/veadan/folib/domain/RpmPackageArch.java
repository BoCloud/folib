package com.veadan.folib.domain;

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
