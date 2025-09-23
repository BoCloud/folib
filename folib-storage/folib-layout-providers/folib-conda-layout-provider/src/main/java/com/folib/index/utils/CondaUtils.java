package com.folib.index.utils;

import com.folib.index.model.MetaYaml;
import com.folib.index.model.RepoDataFileKind;
import com.folib.index.model.RepoDataPackage;
import lombok.Generated;
import org.apache.commons.compress.compressors.CompressorException;
import org.apache.commons.compress.compressors.CompressorOutputStream;
import org.apache.commons.compress.compressors.CompressorStreamFactory;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.ws.rs.core.StreamingOutput;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public class CondaUtils {
    @Generated
    private static final Set<RepoDataFileKind> compressedRepoDataKind;


    public static boolean isMetaData(@Nonnull String path) {
        String fileName = CondaPathUtils.getFileName(path);
        return Stream.of(RepoDataFileKind.values()).anyMatch((repoDataFileKind) -> repoDataFileKind.getFileName().equals(fileName));
    }

    public static boolean isCompressedMetaData(String path) {
        String fileName = CondaPathUtils.getFileName(path);
        return compressedRepoDataKind.stream().anyMatch((repoDataFileKind) -> repoDataFileKind.getFileName().equals(fileName));
    }

    public static String resolveRepoDataPathFromRepoDataCompressedPath(String repoDataCompressedPath) {
        if (StringUtils.isBlank(repoDataCompressedPath)) {
            return null;
        } else {
            String fileName = CondaPathUtils.getFileName(repoDataCompressedPath);
            if (compressedRepoDataKind.stream().noneMatch((repoDataFileKind) -> repoDataFileKind.getFileName().equals(fileName))) {
                return null;
            } else {
                return repoDataCompressedPath.substring(0, repoDataCompressedPath.length() - 4);
            }
        }
    }

    public static Map<String, RepoDataPackage> findMapByName(SortedMap<String, RepoDataPackage> map, String name) {
        Map<String, RepoDataPackage> result = map.entrySet().stream()
                .filter(entry -> entry.getValue().getName().equals(name))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        return result;
    }

    public static Map<String, Map<String, RepoDataPackage>> convertToMapByName(SortedMap<String, RepoDataPackage> map) {
        Map<String, Map<String, RepoDataPackage>> result = new LinkedHashMap<>();
        for (Map.Entry<String, RepoDataPackage> entry : map.entrySet()) {
            String name = entry.getValue().getName();
            if (!result.containsKey(name)) {
                result.put(name, new LinkedHashMap<>());
            }
            result.get(name).put(entry.getKey(), entry.getValue());
        }
        return result;
    }

    public static int getDepth(String path) {
//        return StringUtils.isBlank(path) ? 0 : StringUtils.countMatches(com.folib.conda.utils.PathUtils.trimSlashes(path), '/') + 1;
        return StringUtils.isBlank(path) ? 0 : StringUtils.countMatches(CondaPathUtils.trimSlashes(path), '/') + 1;
    }

    public static void prepareCompressedStream(InputStream repoDataContentInputStream, String compressionType, OutputStream output) throws IOException {
        try {
            try (CompressorOutputStream compressedOut = (new CompressorStreamFactory()).createCompressorOutputStream(compressionType, output)) {
                IOUtils.copyLarge(repoDataContentInputStream, compressedOut);
            }

        } catch (CompressorException e) {
            throw new IOException("Failed to compress repodata.json file contents.", e);
        }
    }

    public static StreamingOutput getStreamingOutput(InputStream repoDataContentInputStream, String compressionType) {
        return (output) -> prepareCompressedStream(repoDataContentInputStream, compressionType, output);
    }

    public static boolean isCondaPackage(String path) {
        return path != null && (path.endsWith(".tar.bz2") || path.endsWith(".conda"));
    }

    public static boolean isNoarchRepodata(String repoDataPath) {
        return repoDataPath != null && (repoDataPath.endsWith("noarch/" + RepoDataFileKind.REPO_DATA.getFileName()) || repoDataPath.endsWith("noarch/" + RepoDataFileKind.CURRENT_REPO_DATA.getFileName()));
    }

    public static boolean isTarBz2File(String path) {
        return path != null && path.endsWith(".tar.bz2");
    }

    public static void splitLine(String line, @Nonnull MetaYaml metaYaml) {
        String[] parts = line.split(":", 2);
        if (parts.length == 2) {
            String key = parts[0].trim();
            String value = parts[1].trim();
            metaYaml.getBuild().put(key, value);
        }

    }

    public static int countLeadingSpaces(String line) {
        return (int)line.chars().takeWhile((c) -> c == 32 || c == 9).count();
    }
    public static String convertCompressionTypeToResponseType(String compressionType) {
        String responseType;
        switch (compressionType) {
            case "bzip2" :
                responseType = "application/x-bzip2";
                break;
            case "zstd" :
                responseType = "application/zstd";
                break;
            default :
                responseType = "*/*";
        }

        return responseType;
    }

    @Nullable
    public static String getCompressorType(String filePath) {
        String extension = CondaPathUtils.getExtension(filePath);
        if (extension == null) {
            return null;
        } else {
            String compressionType;
            switch (extension.toLowerCase()) {
                case "bz2" :
                    compressionType = "bzip2";
                    break;
                case "zst" :
                    compressionType = "zstd";
                    break;
                default :
                    compressionType = null;
            }

            return compressionType;
        }
    }

    @Generated
    private CondaUtils() {
    }

    static {
        compressedRepoDataKind = Set.of(RepoDataFileKind.REPO_DATA_BZ2, RepoDataFileKind.CURRENT_REPO_DATA_BZ2, RepoDataFileKind.REPO_DATA_ZST, RepoDataFileKind.CURRENT_REPO_DATA_ZST);
    }
}
