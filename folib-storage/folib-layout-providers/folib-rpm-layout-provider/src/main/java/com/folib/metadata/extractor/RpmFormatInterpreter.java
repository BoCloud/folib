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


import com.google.common.collect.Lists;

import com.folib.metadata.model.Entry;
import com.folib.metadata.model.File;
import com.folib.util.InternalStringUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.redline_rpm.changelog.ChangelogEntry;
import org.redline_rpm.header.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

public class RpmFormatInterpreter {
    private static final Logger log = LoggerFactory.getLogger(RpmFormatInterpreter.class);
    private final boolean isRequiredEntriesLogicEnabled;

    public RpmFormatInterpreter(boolean isRequiredEntriesLogicEnabled) {
        this.isRequiredEntriesLogicEnabled = isRequiredEntriesLogicEnabled;
    }

    public RpmFormatInterpreter() {
        this.isRequiredEntriesLogicEnabled = false;
    }

    public RpmMetadata interpret(Format rpmFormat) {
        log.trace("Interpreting raw RPM metadata for repository index compatibility");
        RpmMetadata rpmMetadata = new RpmMetadata();
        Signature signature = rpmFormat.getSignature();
        rpmMetadata.setHeaderStart(rpmFormat.getHeader().getStartPos());
        rpmMetadata.setHeaderEnd(rpmFormat.getHeader().getEndPos());
        rpmMetadata.setName(this.getName(rpmFormat));
        if (rpmFormat.getLead().getArch().name().equals(RpmType.SOURCE)) {
            rpmMetadata.setArchitecture( "src");
        } else {
            rpmMetadata.setArchitecture(this.getArchitecture(rpmFormat));
        }
        rpmMetadata.setVersion(this.getVersion(rpmFormat));
        rpmMetadata.setEpoch(this.getEpoch(rpmFormat));
        rpmMetadata.setRelease(this.getRelease(rpmFormat));
        rpmMetadata.setSummary(this.getSummary(rpmFormat));
        rpmMetadata.setDescription(this.getDescription(rpmFormat));
        rpmMetadata.setPackager(this.getPackager(rpmFormat));
        rpmMetadata.setUrl(this.getUrl(rpmFormat));
        rpmMetadata.setBuildTime(this.getBuildTime(rpmFormat));
        rpmMetadata.setInstalledSize(this.getInstalledSize(rpmFormat));
        rpmMetadata.setArchiveSize(this.getArchiveSize(rpmFormat));
        rpmMetadata.setLicense(this.getLicense(rpmFormat));
        rpmMetadata.setVendor(this.getVendor(rpmFormat));
        rpmMetadata.setGroup(this.getGroup(rpmFormat));
        rpmMetadata.setHref(this.getName(rpmFormat) + "-" + this.getVersion(rpmFormat) + "-" + this.getRelease(rpmFormat) + "." + this.getArchitecture(rpmFormat));
        rpmMetadata.setSourceRpm(this.getSourceRpm(rpmFormat));
        rpmMetadata.setBuildHost(this.getBuildHost(rpmFormat));
        rpmMetadata.setProvide(this.resolveEntriesEntries(rpmFormat, Header.HeaderTag.PROVIDENAME, Header.HeaderTag.PROVIDEFLAGS, Header.HeaderTag.PROVIDEVERSION));
        List<Entry> allRequireEntries = this.resolveEntriesEntries(rpmFormat, Header.HeaderTag.REQUIRENAME, Header.HeaderTag.REQUIREFLAGS, Header.HeaderTag.REQUIREVERSION);
        rpmMetadata.setConflict( this.resolveEntriesEntries(rpmFormat, Header.HeaderTag.CONFLICTNAME, Header.HeaderTag.CONFLICTFLAGS, Header.HeaderTag.CONFLICTVERSION));
        rpmMetadata.setObsolete( this.resolveEntriesEntries(rpmFormat, Header.HeaderTag.OBSOLETENAME, Header.HeaderTag.OBSOLETEFLAGS, Header.HeaderTag.OBSOLETEVERSION));
        rpmMetadata.setFiles( this.resolveFiles(rpmFormat));
        rpmMetadata.setChangeLogs(this.resolveChangeLogs(rpmFormat));
        if (this.isRequiredEntriesLogicEnabled) {
            rpmMetadata.setRequire(this.applyRpmRequireEntriesLogic(allRequireEntries, rpmMetadata.getProvide(), rpmMetadata.getFiles()));
        } else {
            rpmMetadata.setRequire(allRequireEntries);
        }

        log.trace("Completed interpretation of raw RPM metadata for repository index compatibility");
        return rpmMetadata;
    }

    private String getName(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.NAME);
    }

    private String getArchitecture(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.ARCH);
    }

    private String getVersion(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.VERSION);
    }

    private int getEpoch(Format rpmFormat) {
        return this.getIntHeader(rpmFormat, Header.HeaderTag.EPOCH);
    }

    private String getRelease(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.RELEASE);
    }

    private String getSummary(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.SUMMARY);
    }

    private String getDescription(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.DESCRIPTION);
    }

    private String getPackager(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.PACKAGER);
    }

    private String getUrl(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.URL);
    }

    private int getBuildTime(Format rpmFormat) {
        return this.getIntHeader(rpmFormat, Header.HeaderTag.BUILDTIME);
    }

    private int getInstalledSize(Format rpmFormat) {
        return this.getIntHeader(rpmFormat, Header.HeaderTag.SIZE);
    }

    private int getArchiveSize(Format rpmFormat) {
        return this.getIntHeader(rpmFormat.getSignature(), Signature.SignatureTag.PAYLOADSIZE);
    }

    private String getLicense(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.LICENSE);
    }

    private String getVendor(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.VENDOR);
    }

    private String getGroup(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.GROUP);
    }

    private String getSourceRpm(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.SOURCERPM);
    }

    private String getBuildHost(Format rpmFormat) {
        return this.getStringHeader(rpmFormat, Header.HeaderTag.BUILDHOST);
    }

    private LinkedList<Entry> resolveEntriesEntries(Format rpmFormat, Header.HeaderTag namesTag, Header.HeaderTag flagsTag, Header.HeaderTag versionsTag) {
        LinkedList<Entry> entries = Lists.newLinkedList();
        String[] entryNames = this.getStringArrayHeader(rpmFormat, namesTag);
        if (entryNames != null) {
            int[] entryFlags = this.getIntArrayHeader(rpmFormat, flagsTag);
            String[] entryVersions = this.getStringArrayHeader(rpmFormat, versionsTag);

            for (int i = 0; i < entryNames.length; ++i) {
                String entryName = entryNames[i];
                Entry entry = new Entry();

                entry.name = entryName;
                if (entryFlags.length > i) {
                    int entryFlag = entryFlags[i];
                    this.setEntryFlags(entryFlag, entry);
                    if ((entryFlag & Flags.PREREQ) > 0 || (entryFlag & Flags.SCRIPT_PRE) > 0 || (entryFlag & Flags.SCRIPT_POST) > 0) {
                        entry.pre = "1";
                    }
                }
                if (entryVersions.length > i) {
                    this.setEntryVersionFields(entryVersions[i], entry);
                }

                entries.add(entry);
            }
        }

        return entries;
    }

    List<Entry> applyRpmRequireEntriesLogic(List<Entry> allEntries, List<Entry> providedList, List<File> files) {
        List<Entry> filteredEntries = this.removeDuplicatesFromEntries(allEntries);
        List<Entry> libcSoEntries = new LinkedList();
        filteredEntries = (List) filteredEntries.stream().filter((entry) -> {
            String entryName = entry.name;
            if (StringUtils.isNotEmpty(entryName) && entryName.startsWith("libc.so.6")) {
                libcSoEntries.add(entry);
            }

            return !this.isPrefixNameExcluded(entryName) && !this.isPrimaryFile(entryName, files) && !this.isProvidedFile(entry, providedList);
        }).collect(Collectors.toList());
        Entry libcSoLatestEntry = this.findLatestLibcSoEntry(libcSoEntries);
        if (libcSoLatestEntry != null) {
            filteredEntries.add(libcSoLatestEntry);
        }

        return filteredEntries;
    }

    private boolean isPrefixNameExcluded(String entryName) {
        if (!StringUtils.isNotEmpty(entryName)) {
            return false;
        } else {
            return entryName.startsWith("libc.so.6") || entryName.startsWith("rpmlib");
        }
    }

    //private List<Entry> removeDuplicatesFromEntries(List<Entry> listWithDuplicates) {
    //    TreeSet<Entry> uniqueList = (TreeSet<Entry>)listWithDuplicates.stream().collect(Collectors.toCollection(() -> new TreeSet(RpmFormatInterpreter::compare)));
    //    return new LinkedList<>(uniqueList);
    //}

    private List<Entry> removeDuplicatesFromEntries(List<Entry> listWithDuplicates) {
        // 过滤掉 null 元素
        List<Entry> filteredList = listWithDuplicates.stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        // 使用 TreeSet 去重并排序
        TreeSet<Entry> uniqueSet = new TreeSet<>(Comparator.comparing((Entry e) -> e.name, InternalStringUtils::compareNullLast)
                .thenComparing(e -> e.flags, InternalStringUtils::compareNullLast)
                .thenComparing(e -> e.version, InternalStringUtils::compareNullLast)
                .thenComparing(e -> e.pre, InternalStringUtils::compareNullLast));

        uniqueSet.addAll(filteredList);

        // 返回新的 LinkedList
        return new LinkedList<>(uniqueSet);
    }


    private static int compare(Entry o1, Entry o2) {
        return InternalStringUtils.compareNullLast(o1.name, o2.name) |
                InternalStringUtils.compareNullLast(o1.flags, o2.flags) |
                InternalStringUtils.compareNullLast(o1.version, o2.version) |
                InternalStringUtils.compareNullLast(o1.pre, o2.pre);
    }

    private Entry findLatestLibcSoEntry(List<Entry> libcSoEntries) {
        return libcSoEntries.isEmpty() ? null : (Entry) Collections.max(libcSoEntries, this::compareGlibcVersions);
    }

    private int compareGlibcVersions(Entry entry1, Entry entry2) {
        String version1AsString = StringUtils.substringBetween(entry1.name, "(GLIBC_", ")");
        String version2AsString = StringUtils.substringBetween(entry2.name, "(GLIBC_", ")");
        if (StringUtils.isNotEmpty(version1AsString) && StringUtils.isNotEmpty(version2AsString)) {
            DefaultArtifactVersion version1 = new DefaultArtifactVersion(version1AsString);
            DefaultArtifactVersion version2 = new DefaultArtifactVersion(version2AsString);
            return version1.compareTo(version2);
        } else {
            return InternalStringUtils.compareNullLast(version1AsString, version2AsString);
        }
    }

    private boolean isProvidedFile(Entry entry, List<Entry> providedList) {
        String entryIdentifier = this.getUniqueEntryIdentifier(entry);
        return providedList.stream().anyMatch((e) -> {
            return this.getUniqueEntryIdentifier(e).equals(entryIdentifier);
        });
    }

    private String getUniqueEntryIdentifier(Entry entry) {
        return String.join("-", entry.name, entry.flags, entry.version, entry.pre);
    }

    private boolean isPrimaryFile(String entryName, List<File> files) {
        return this.isEntryInFiles(entryName, files) && (entryName.startsWith("/etc/") || entryName.equals("/usr/lib/sendmail"));
    }

    private boolean isEntryInFiles(String entryName, List<File> files) {
        try {
            return files.stream().anyMatch((file) -> {
                return file.path.equals(entryName);
            });
        } catch (Exception var4) {
            return false;
        }
    }

    private void setEntryFlags(int entryFlags, Entry entry) {
        if ((entryFlags & Flags.LESS) > 0 && (entryFlags & Flags.EQUAL) > 0) {
            entry.flags = "LE";
        } else if ((entryFlags & Flags.GREATER) > 0 && (entryFlags & Flags.EQUAL) > 0) {
            entry.flags = "GE";
        } else if ((entryFlags & Flags.EQUAL) > 0) {
            entry.flags = "EQ";
        } else if ((entryFlags & Flags.LESS) > 0) {
            entry.flags = "LT";
        } else if ((entryFlags & Flags.GREATER) > 0) {
            entry.flags = "GT";
        }

    }

    private void setEntryVersionFields(String entryVersion, Entry entry) {
        if (StringUtils.isNotBlank(entryVersion)) {
            String[] versionTokens = StringUtils.split(entryVersion, '-');
            String versionValue = versionTokens[0];
            String[] versionValueTokens = StringUtils.split(versionValue, ':');
            if (versionValueTokens.length > 1) {
                entry.epoch = versionValueTokens[0];
                entry.version = versionValueTokens[1];
            } else {
                entry.epoch = "0";
                entry.version = versionValueTokens[0];
            }

            if (versionTokens.length > 1) {
                String releaseValue = versionTokens[1];
                if (StringUtils.isNotBlank(releaseValue)) {
                    entry.release = releaseValue;
                }
            }
        }

    }

    private LinkedList<File> resolveFiles(Format rpmFormat) {
        LinkedList<File> files = Lists.newLinkedList();
        String[] baseNames = this.getStringArrayHeader(rpmFormat, Header.HeaderTag.BASENAMES);
        int[] baseNameDirIndexes = this.getIntArrayHeader(rpmFormat, Header.HeaderTag.DIRINDEXES);
        List<String> dirPaths = Lists.newArrayList(this.getStringArrayHeader(rpmFormat, Header.HeaderTag.DIRNAMES));

        for (int i = 0; i < baseNames.length; ++i) {
            String baseName = baseNames[i];
            int baseNameDirIndex = baseNameDirIndexes[i];
            String var10000 = (String) dirPaths.get(baseNameDirIndex);
            String filePath = var10000 + baseName;
            boolean dir = dirPaths.contains(filePath + "/");
            File file = new File();
            file.path = filePath;
            file.type = dir ? "dir" : null;
            files.add(file);
        }

        return files;
    }

    private LinkedList<ChangelogEntry> resolveChangeLogs(Format rpmFormat) {
        LinkedList<ChangelogEntry> changeLogs = Lists.newLinkedList();
        String[] changeLogAuthors = this.getStringArrayHeader(rpmFormat, Header.HeaderTag.CHANGELOGNAME);
        int[] changeLogDates = this.getIntArrayHeader(rpmFormat, Header.HeaderTag.CHANGELOGTIME);
        String[] changeLogTexts = this.getStringArrayHeader(rpmFormat, Header.HeaderTag.CHANGELOGTEXT);

        for (int i = 0; i < changeLogTexts.length; ++i) {
            ChangelogEntry changeLog = new ChangelogEntry();
            changeLog.setUserMakingChange( changeLogAuthors[i]);
            int timestampInt = changeLogDates[i]; // 例如，这个整数代表某个时间点
            long timestampLong = (long) timestampInt * 1000; // 如果是秒数，则需要乘以1000
            Date date = new Date(timestampLong);
            changeLog.setChangeLogTime(date);
            changeLog.setDescription(changeLogTexts[i]);
            changeLogs.add(changeLog);
        }

        return changeLogs;
    }

    private String getStringHeader(Format format, Header.HeaderTag tag) {
        if (format.getHeader().getEntry(tag) != null) {
            String[] values = (String[]) format.getHeader().getEntry(tag).getValues();
            return values != null && values.length >= 1 ? values[0] : null;
        }
        return null;
    }
    private  String[] getStringArrayHeader(Format format, Header.HeaderTag tag) {
        if (format.getHeader().getEntry(tag) != null) {
            String[] values = (String[]) format.getHeader().getEntry(tag).getValues();
            return values != null && values.length >= 1 ? values : new String[0];
        }
        return new String[0];
    }

    private int getIntHeader(Format rpmFormat, Header.HeaderTag tag) {
        int[] values = this.getIntArrayHeader(rpmFormat, tag);
        return values != null && values.length >= 1 ? values[0] : 0;
    }

    private int getIntHeader(Format rpmFormat,  Signature.SignatureTag tag) {
        int[] values = this.getIntArrayHeader(rpmFormat, tag);
        return values != null && values.length >= 1 ? values[0] : 0;
    }

    private int getIntHeader(Signature signature,  Signature.SignatureTag tag) {
        int[] values = this.getIntArrayHeader(signature, tag);
        return values != null && values.length >= 1 ? values[0] : 0;
    }


    private int[] getIntArrayHeader(Format rpmFormat, Signature.SignatureTag tag) {
        // 获取 HeaderTag 对应的整型值
        if (rpmFormat.getHeader().getEntry(tag) != null) {
            int[] values = (int[]) rpmFormat.getHeader().getEntry(tag).getValues();
            return values != null && values.length > 0 ? values : new int[]{};
        }
        return new int[]{};
    }

    private int[] getIntArrayHeader(Signature signature, Signature.SignatureTag tag) {
        // 获取 HeaderTag 对应的整型值
        if (signature.getEntry(tag) != null) {
            int[] values = (int[]) signature.getEntry(tag).getValues();
            return values != null && values.length > 0 ? values : new int[]{};
        }
        return new int[]{};
    }

    private int[] getIntArrayHeader(Format rpmFormat, Header.HeaderTag tag) {
        // 获取 HeaderTag 对应的整型值
        if (rpmFormat.getHeader().getEntry(tag) != null) {
            int[] values = (int[]) rpmFormat.getHeader().getEntry(tag).getValues();
            return values != null && values.length > 0 ? values : new int[]{};
        }
        return new int[]{};
    }


}

