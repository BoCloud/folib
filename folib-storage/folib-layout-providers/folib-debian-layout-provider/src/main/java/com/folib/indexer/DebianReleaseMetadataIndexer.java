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
package com.folib.indexer;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.folib.constant.DebianConstant;
import com.folib.domain.Artifact;
import com.folib.domain.DebianReleaseContext;
import com.folib.domain.DebianReleaseMetadataEntry;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.storage.repository.Repository;
import com.folib.util.DebianUtils;
import com.folib.util.steam.StringInputStream;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TimeZone;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 * @since 2024-09-04 16:18
 */
@Slf4j
public class DebianReleaseMetadataIndexer {
    private static final String DATE_TIME_FORMAT = "EEE, dd MMM yyyy HH:mm:ss";
    private static final String COMPONENT_AND_ARCHITECTURE_PATTERN = "([\\w/-]*)/binary-(.*)/[\\w\\d.]*";
    private final Repository repo;
    private final List<String> packagesFilesToDelete;

    private final RepositoryPathResolver resolver;
    private final Predicate<String> anyAndAll = (input) -> input != null && !input.equals("all") && !input.equals("any");
    private final Comparator<DebianReleaseMetadataEntry> orderByPath = Comparator.comparing((entry) -> entry.path);


    public DebianReleaseMetadataIndexer(Repository repo, List<String> packagesFilesToDelete, RepositoryPathResolver resolver) {
        this.repo = repo;
        this.resolver = resolver;
        this.packagesFilesToDelete = packagesFilesToDelete != null ? packagesFilesToDelete : Lists.newArrayList();
    }

//    public List<Artifact> indexRelease(DebianReleaseContext initialContext) {
//        // 去掉空的package
//        List<Artifact> packages = this.resolvePackagesFilesToIndex(initialContext);
//        DebianReleaseContext releaseContext = this.createReleaseContextByPackagesArtifacts(initialContext, packages);
//        List<DebianReleaseMetadataEntry> releaseEntryList = packages.stream().filter(Objects::nonNull).map(artifact -> this.createReleaseEntry(artifact, releaseContext)).sorted(this.orderByPath).collect(Collectors.toList());
//        try {
//            this.writeIndexFiles(releaseEntryList, releaseContext);
//            return packages;
//        } catch (Exception e) {
//            throw new RuntimeException(e);
//        }
//    }

    private void writeIndexFiles(List<DebianReleaseMetadataEntry> releaseEntries, DebianReleaseContext releaseContext) throws Exception {
        if (releaseEntries == null) {
            throw new NullPointerException("releaseEntries is marked non-null but is null");
        } else if (releaseContext == null) {
            throw new NullPointerException("releaseContext is marked non-null but is null");
        } else {
            String releaseFileContent = this.getReleaseContent(releaseEntries, releaseContext);
//            this.writeInRelease(releaseContext, releaseFileContent);
            String releaseFilePath = this.pathToReleaseFile(releaseContext, "Release");
            this.write(releaseFileContent, releaseFilePath, "Release");
//            this.writeReleaseSignature(releaseContext, releaseFileContent);
        }
    }

    // todo inrelease 需要公私钥配置 目前无此功能
    private void writeInRelease(DebianReleaseContext releaseContext, String releaseFileContent) throws Exception {
//        if (releaseContext == null) {
//            throw new NullPointerException("releaseContext is marked non-null but is null");
//        } else if (releaseFileContent == null) {
//            throw new NullPointerException("releaseFileContent is marked non-null but is null");
//        } else {
//            String privateKey =this.repo.getRepositoryConfiguration().getSigningPrivateKey();
//            String password = this.repo.getRepositoryConfiguration().getSigningPassword();
//            if (StringUtils.isNotBlank(privateKey) && password != null) {
//                InputStream relFile = new StringInputStream(releaseFileContent);
//
//                try {
//                    String inReleaseFilePath = this.pathToReleaseFile(releaseContext, "InRelease");
//                    log.debug("Creating InRelease file index at path {}", inReleaseFilePath);
//                    String inReleaseFileContent = GpgSigner.signFile(privateKey, password, true, relFile);
//                    String finalInReleaseFile = this.fixInReleaseLineSeparators(inReleaseFileContent, System.lineSeparator());
//                    this.write(finalInReleaseFile, inReleaseFilePath, "InRelease");
//                } catch (Throwable var10) {
//                    try {
//                        relFile.close();
//                    } catch (Throwable var9) {
//                        var10.addSuppressed(var9);
//                    }
//
//                    throw var10;
//                }
//
//                relFile.close();
//            } else {
//                log.debug("Skipping the creation of InRelease file: missing private signing key or password");
//                String inReleaseFilePath = !releaseContext.isAutomaticLayout() ? "InRelease" : DebianUtils.getContextPath(String.format("%s/%s", releaseContext.getReleasePath(), "InRelease"));
//                if (this.repo.exists(inReleaseFilePath)) {
//                    log.info("Found existing InRelease file without private signing key or password, removing the InRelease");
//                    this.repo.delete(inReleaseFilePath);
//                }
//            }
//
//        }

    }

    protected String fixInReleaseLineSeparators(String inReleaseFile, String lineSeparator) {
        return lineSeparator.equals("\n") ? StringUtils.trim(inReleaseFile) : StringUtils.trim(inReleaseFile.replace(lineSeparator, "\n"));
    }


    //
//    private List<Artifact> resolvePackagesFilesToIndex(DebianReleaseContext initialContext) {
//
//        packages.removeIf(pkg -> this.packagesFilesToDelete.contains(pkg.getArtifactPath()));
//        return packages;
//
//    }

    private DebianReleaseContext createReleaseContextByPackagesArtifacts(DebianReleaseContext context, List<Artifact> packages) {
        Set<String> components = Sets.newHashSet();
        Set<String> architectures = Sets.newHashSet();
        this.addComponentsAndArchitectures(packages, context, components, architectures);
        architectures.remove("all");
        architectures.remove("any");
        String distribution = context.getDistribution();
        try {
            return new DebianReleaseContext(distribution, components, architectures);
        } catch (IllegalArgumentException e) {
            log.error("Error while assembling index coordinates for distribution {}, Release index will not be created", distribution);
            return null;
        }
    }

    private void addComponentsAndArchitectures(List<Artifact> packages, DebianReleaseContext context, Set<String> components, Set<String> architectures) {
        for (Artifact artifact : packages) {
            String adjustedPath = artifact.getArtifactPath().replace(this.getTempPathWithDistro(context), "");
            String relativePath = Optional.ofNullable(DebianUtils.trimLeadingSlashes(StringUtils.removeStart(adjustedPath, context.getReleasePath()))).orElse("");
            Matcher matcher = Pattern.compile(COMPONENT_AND_ARCHITECTURE_PATTERN).matcher(relativePath);
            if (matcher.find()) {
                components.add(matcher.group(1));
                architectures.add(matcher.group(2));
            } else {
                log.warn("Invalid Packages file path, cannot add to Release index : {}", relativePath);
            }
        }

    }

//    private DebianReleaseMetadataEntry createReleaseEntry(Artifact artifact, DebianReleaseContext context) {
//        String relativePath;
//
//        String artifactPath = artifact.getArtifactPath();
//        String releasePath = context.getReleasePath();
//        relativePath = StringUtils.removeStart(artifactPath, releasePath + "/" + this.getTempPathWithDistro(context));
//        relativePath = StringUtils.removeStart(relativePath, context.getReleasePath());
//
//        relativePath = DebianUtils.trimSlashes(relativePath);
//        String md5 = artifact.getChecksums().get("MD5");
//        return new DebianReleaseMetadataEntry(relativePath, artifact, md5);
//    }

    private DebianReleaseMetadataEntry createReleaseEntry(RepositoryPath repositoryPath, DebianReleaseContext context) {
        Repository repository = repositoryPath.getRepository();
        RepositoryPath releasePath = resolver.resolve(repository,context.getReleasePath());
        String relativePath = repositoryPath.getPath().substring(releasePath.getPath().length()+1);
        // 写md5
        relativePath = DebianUtils.trimSlashes(relativePath);
        DebianReleaseMetadataEntry entry = new DebianReleaseMetadataEntry();
        entry.setPath(relativePath);
        try {
            entry.setSize(Files.size(repositoryPath));
            entry.setSha1(Files.readString(Paths.get(repositoryPath+".sha1")));
            entry.setSha256(Files.readString(Paths.get(repositoryPath+".sha256")));
            entry.setMd5sum(Files.readString(Paths.get(repositoryPath+".md5")));
        }catch (Exception e) {
            log.info("Error while getting release size for release path {}", relativePath, e);
        }
        return entry;

    }

    private void write(String releaseFileContent, String releaseFilePath, String name) throws Exception {
        log.info("Writing Debian {} index at path {}", name, releaseFilePath);
        try (InputStream relContent = new StringInputStream(releaseFileContent)) {
            RepositoryPath repositoryPath = resolver.resolve(this.repo, releaseFilePath);
            Files.copy(relContent, repositoryPath, StandardCopyOption.REPLACE_EXISTING);
            log.debug("Finished writing Debian {} index at path {}.", name, releaseFilePath);
        } catch (RuntimeException e) {
            log.error("write release failed{}",e.getMessage(),e);
            throw e;
        }

    }

    // todo release signature 需要公私钥配置 目前无此功能
    private void writeReleaseSignature(DebianReleaseContext releaseContext, String releaseFileContent) throws Exception {


    }

    private String getReleaseContent(List<DebianReleaseMetadataEntry> releaseEntries, DebianReleaseContext releaseContext) {
        StringBuilder relBuilder = new StringBuilder();
        relBuilder.append("Origin: ").append("FOLIB").append("\n");
        relBuilder.append("Label: ").append("FOLIB").append("\n");
        relBuilder.append("Suite: ").append(StringUtils.defaultString(releaseContext.getDistribution())).append("\n");
        relBuilder.append("Codename: ").append(StringUtils.defaultString(releaseContext.getDistribution())).append("\n");
        relBuilder.append("Date: ").append(this.getCurrentDateString()).append("\n");
//        if (workContext.isUseAcquireByHash() && releaseContext.isAutomaticLayout()) {
//            relBuilder.append("Acquire-By-Hash: yes\n");
//        }
        this.writeComponents(releaseContext, relBuilder);
        this.writeArchitectures(releaseContext, relBuilder);
        this.writeChecksumEntries(releaseEntries, relBuilder);
        return relBuilder.toString();
    }

    private void writeComponents(DebianReleaseContext releaseContext, StringBuilder relBuilder) {
        StringBuilder componentsKey = new StringBuilder("Components: ");
        String[] components = releaseContext.getComponents();
        for (String component : components) {
            componentsKey.append(component).append(" ");
        }
        relBuilder.append(componentsKey.toString().trim()).append("\n");
    }

    private void writeArchitectures(DebianReleaseContext releaseContext, StringBuilder relBuilder) {
        List<String> architectures = Arrays.stream(releaseContext.getArchitectures()).filter(this.anyAndAll).sorted().collect(Collectors.toList());
        StringBuilder architecturesKey = new StringBuilder(architectures.size() > 1 ? "Architectures: " : "Architecture: ");
        for (String architecture : architectures) {
            architecturesKey.append(architecture).append(" ");
        }
        relBuilder.append(architecturesKey.toString().trim()).append("\n");
    }

    private void writeChecksumEntries(List<DebianReleaseMetadataEntry> releaseEntries, StringBuilder relBuilder) {
        Checksum[] checksums = DebianReleaseMetadataIndexer.Checksum.values();
        for (Checksum checksum : checksums) {
            if (!releaseEntries.isEmpty()) {
                relBuilder.append(checksum.header);
                for (DebianReleaseMetadataEntry entry : releaseEntries) {
                    this.writeMetadataEntry(relBuilder, entry, checksum);
                }
            }
        }
    }

    private void writeMetadataEntry(StringBuilder relBuilder, DebianReleaseMetadataEntry entry, Checksum checksum) {
        String digest = this.entryChecksum(entry, checksum);
        relBuilder.append(" ").append(digest).append(StringUtils.leftPad("" + entry.size, 17)).append(" ").append(entry.path).append('\n');
    }

    private String entryChecksum(DebianReleaseMetadataEntry entry, Checksum checksum) {
        switch (checksum) {
            case MD5:
                return entry.md5sum;
            case SHA1:
                return entry.sha1;
            case SHA2:
                return entry.sha256;
            default:
                return null;
        }
    }

    private String getCurrentDateString() {
        SimpleDateFormat formatter = new SimpleDateFormat(DATE_TIME_FORMAT, Locale.ENGLISH);
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        return formatter.format(new Date()) + " UTC";
    }

    private RepositoryPath getPackagesArtifacts(DebianReleaseContext releaseContext) {
        String releasePath = Joiner.on("/").join("dists", releaseContext.getDistribution());
        return resolver.resolve(this.repo, releasePath);
    }

    private Predicate<Artifact> isSamePackagesIndexFile(DebianReleaseContext initialContext, Artifact newPkg) {
        return (pkg) -> pkg.getArtifactPath().equals(newPkg.getArtifactPath().replace(this.getTempPathWithDistro(initialContext), ""));
    }

    private String pathToReleaseFile(DebianReleaseContext context, String filename) {
        return String.format("%s/%s", context.getReleasePath(), filename);

    }

    private String getTempPathWithDistro(DebianReleaseContext context) {
        return DebianConstant.PACKAGE_PREFIX + "/" + context.getDistribution() + "/";
    }

    private static enum Checksum {
        MD5("MD5Sum:\n"),
        SHA1("SHA1:\n"),
        SHA2("SHA256:\n");
        public String header;

        private Checksum(String header) {
            this.header = header;
        }
    }


    public void indexRelease(String distribution) {
        // 获取新的所有的package
        String path= Joiner.on("/").join("dists", distribution);
        RepositoryPath repositoryPath = resolver.resolve(this.repo, path);
        try(Stream<Path> paths = Files.walk(repositoryPath)){
            List<Path> packages = paths.map(p->(RepositoryPath)p).filter(p -> {
                try {
                    return !p.toString().contains("by-hash")&& !RepositoryFiles.isChecksum((p))&&!RepositoryFiles.isArtifactMetadata(p)&&!Files.isDirectory(p)&&!"Release".equals(p.getFileName().toString());
                } catch (IOException e) {
                    return false;
                }
            }).collect(Collectors.toList());
            packages.removeIf(pkg -> this.packagesFilesToDelete.contains(pkg.toString()));

            Set<String> components = Sets.newHashSet();
            Set<String> architectures = Sets.newHashSet();
            Set<Path> correctPackages = Sets.newHashSet();
            for (Path pgk : packages) {
                String relativePath = repositoryPath.relativize(pgk).toString();
                Matcher matcher = Pattern.compile(COMPONENT_AND_ARCHITECTURE_PATTERN).matcher(relativePath);
                if (matcher.find()) {
                    components.add(matcher.group(1));
                    architectures.add(matcher.group(2));
                    correctPackages.add(pgk);
                } else {
                    log.warn("Invalid Packages file path, cannot add to Release index : {}", pgk);
                }
            }

            architectures.remove("all");
            architectures.remove("any");
            DebianReleaseContext releaseContext = new DebianReleaseContext(distribution, components, architectures);
            List<DebianReleaseMetadataEntry> releaseEntryList = correctPackages.stream().filter(Objects::nonNull).map(p -> {
                try {
                        RepositoryPath repoPath =(RepositoryPath)p;
                        return this.createReleaseEntry(repoPath, releaseContext);
                } catch (Exception e) {
                    log.error(e.getMessage());
                    return null;
                }
            }).filter(Objects::nonNull).sorted(this.orderByPath).collect(Collectors.toList());
            this.writeIndexFiles(releaseEntryList, releaseContext);
        }catch (Exception e){
            log.info("release index failed : {}", e.getMessage(),e);
        }
    }
}
