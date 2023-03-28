package com.veadan.folib.utils;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.artifact.archive.JarArchiveListingFunction;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * @author leipenghui
 * @date 2022/10/24
 **/
@Slf4j
public class ArtifactUtils {

    /**
     * 判断路径是否以某种后缀结尾
     *
     * @param path       路径
     * @param suffixList 后缀列表
     * @return true
     */
    private static boolean endsWith(String path, List<String> suffixList) {
        boolean flag = false;
        if (StringUtils.isNotBlank(path) && CollectionUtils.isNotEmpty(suffixList)) {
            for (String item : suffixList) {
                if (path.endsWith(item)) {
                    flag = true;
                    break;
                }
            }
        }
        return flag;
    }

    /**
     * 安全扫描 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @return true 支持 false 不支持
     */
    public static boolean layoutSupportsForScan(RepositoryPath repositoryPath) {
        return layoutSupports(repositoryPath, false, true);
    }

    /**
     * 漏洞阻断 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @return true 支持 false 不支持
     */
    public static boolean layoutSupportsForBlock(RepositoryPath repositoryPath) {
        return layoutSupports(repositoryPath, true, false);
    }

    /**
     * 通用 docker 支持镜像版本 maven 支持pom
     * 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @return true 支持 false 不支持
     */
    public static boolean layoutSupports(RepositoryPath repositoryPath) {
        return layoutSupports(repositoryPath, false, false);
    }

    /**
     * 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @param block          阻断 true
     * @param scan           安全扫描 true
     * @return true 支持 false 不支持
     */
    public static boolean layoutSupports(RepositoryPath repositoryPath, Boolean block, Boolean scan) {
        boolean flag = false;
        if (Objects.nonNull(repositoryPath)) {
            if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
                log.debug("=====>>>>> docker布局");
                String blobs = "blobs";
                String manifest = "manifest";
                String path = repositoryPath.toAbsolutePath().toString();
                if (Boolean.TRUE.equals(block)) {
                    if (path.contains("sha256") && !path.endsWith(".sha256")) {
                        return true;
                    }
                } else if (path.contains("sha256") && !path.contains(blobs) && !path.contains(manifest) && !path.endsWith(".sha256")) {
                    return true;
                }
            } else if (repositoryPath.getFileSystem() instanceof MavenFileSystem) {
                log.debug("=====>>>>> maven布局");
                if (Boolean.TRUE.equals(scan)) {
                    flag = JarArchiveListingFunction.INSTANCE.supports(repositoryPath);
                } else {
                    flag = JarArchiveListingFunction.INSTANCE.supports(repositoryPath) || endsWith(repositoryPath.getFileName().toString(), Collections.singletonList(".pom"));
                }
            } else if (repositoryPath.getFileSystem() instanceof NpmFileSystem) {
                log.debug("=====>>>>> npm布局");
                List<String> suffixList = Arrays.asList(".json", ".tgz");
                flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
            } else if (repositoryPath.getFileSystem() instanceof NugetFileSystem) {
                log.debug("=====>>>>> nuget布局");
                List<String> suffixList = Arrays.asList(".nupkg", ".nuspec", "packages.config");
                flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
            } else if (repositoryPath.getFileSystem() instanceof PypiFileSystem) {
                log.debug("=====>>>>> pypi布局");
                List<String> suffixList = Arrays.asList(".whl", ".egg", ".zip");
                flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
            } else if (repositoryPath.getFileSystem() instanceof RawFileSystem) {
                log.debug("=====>>>>> raw布局");
                return true;
            } else if (repositoryPath.getFileSystem() instanceof RpmFileSystem) {
                log.debug("=====>>>>> rpm布局");
                List<String> suffixList = Arrays.asList(".rpm");
                flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
            } else if (repositoryPath.getFileSystem() instanceof PhpFileSystem) {
                log.debug("=====>>>>> php布局");
                List<String> suffixList = Arrays.asList("tar", "tar.gz", "tar.bz2", "zip", "json");
                flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
            } else if (repositoryPath.getFileSystem() instanceof ConanFileSystem) {
                log.debug("=====>>>>> Conan布局");
                List<String> suffixList = Arrays.asList(".tgz", ".py");
                flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
            } else if (repositoryPath.getFileSystem() instanceof HelmFileSystem) {
                List<String> suffixList = Arrays.asList(".tgz");
                flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
                log.debug("=====>>>>> Helm布局");
            }
        }
        log.debug("=====>>>>> 是否是该布局支持的类型：{}", flag);
        return flag;
    }

    /**
     * 拼接url
     *
     * @param repositoryBaseUrl url前缀
     * @param path              url路径
     * @return 拼接后的url
     */
    public static String escapeUrl(String repositoryBaseUrl, String path) {
        String baseUrl = repositoryBaseUrl + (repositoryBaseUrl.endsWith("/") ? "" : "/");
        String p = (path.startsWith("/") ? path.substring(1, path.length()) : path);
        return baseUrl + p;
    }

    /**
     * 获取制品元数据
     *
     * @param artifact artifact
     * @return 制品元数据
     */
    public static JSONObject getMetadata(Artifact artifact) {
        if (Objects.isNull(artifact)) {
            return null;
        }
        String metadata = artifact.getMetadata();
        JSONObject metadataJson = null;
        if (StringUtils.isNotBlank(metadata)) {
            metadataJson = JSONObject.parseObject(metadata);
        }
        return metadataJson;
    }

    /**
     * 获取docker制品镜像名称
     *
     * @param artifactPath 制品路径
     * @return docker制品镜像名称
     */
    public static String getDockerImage(String artifactPath) {
        if (StringUtils.isBlank(artifactPath)) {
            return "";
        }
        String artifactName = artifactPath.substring(0, artifactPath.indexOf("/sha256"));
        String separator = "/";
        boolean isDockerLayout;
        String[] dockerArr;
        if (artifactName.contains(separator)) {
            dockerArr = artifactName.split(separator);
            artifactName = dockerArr[0] + ":" + dockerArr[1];
        }
        return artifactName;
    }
}
