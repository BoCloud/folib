package com.veadan.folib.metadata.indexer.xml;

import lombok.Getter;
import lombok.Setter;

import java.util.Objects;

@Getter
@Setter
// 软件包对象
public class Package {
    private String type;
    private String name;
    private String arch;
    private Version version;
    private Checksum checksum;
    private String summary;
    private String description;
    private String packager;
    private String url;
    private Time time;
    private Size size;
    private Location location;
    private Format format;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Package aPackage = (Package) o;
        return Objects.equals(version, aPackage.version) &&
                Objects.equals(name, aPackage.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(version, name);
    }
}
