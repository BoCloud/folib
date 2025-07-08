package com.folib.metadata.extractor;

import com.folib.metadata.model.Entry;
import com.folib.metadata.model.File;
import lombok.Data;
import org.redline_rpm.changelog.ChangelogEntry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.util.List;


@Data
public class RpmMetadata implements Serializable {

    private static final Logger log = LoggerFactory.getLogger(RpmMetadata.class);
    private String sha1Digest;
    private String artifactRelativePath;
    private long lastModified;
    private long size;
    private int headerStart;
    private int headerEnd;
    private String name;
    private String architecture;
    private String version;
    private int epoch;
    private String release;
    private String summary;
    private String description;
    private String packager;
    private String url;
    private int buildTime;
    private int installedSize;
    private int archiveSize;
    private String license;
    private String vendor;
    private String sourceRpm;
    private String buildHost;
    private String href;
    private String group;
    private List<Entry> provide;
    private List<Entry> require;
    private List<Entry> conflict;
    private List<Entry> obsolete;
    private List<Entry> recommends;
    private List<Entry> suggests;
    private List<File> files;
    private List<ChangelogEntry> changeLogs;
}
