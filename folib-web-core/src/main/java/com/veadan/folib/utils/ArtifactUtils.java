package com.veadan.folib.utils;

import com.veadan.folib.artifact.archive.JarArchiveListingFunction;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.List;

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
     * 校验制品类型是否是该布局支持的类型
     *
     * @param repositoryPath 仓库地址
     * @return true 支持 false 不支持
     */
    public static boolean layoutSupports(RepositoryPath repositoryPath) {
        boolean flag = false;
        if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
            log.info("=====>>>>> docker布局");
            String blobs = "blobs";
            String manifest = "manifest";
            String path = repositoryPath.toAbsolutePath().toString();
            //docker布局
            if (!path.contains(blobs) && !path.contains(manifest) && !path.endsWith(".sha256")) {
                return true;
            }
        } else if (repositoryPath.getFileSystem() instanceof MavenFileSystem) {
            log.info("=====>>>>> maven布局");
            //maven布局
            flag = JarArchiveListingFunction.INSTANCE.supports(repositoryPath);
        } else if (repositoryPath.getFileSystem() instanceof NpmFileSystem) {
            log.info("=====>>>>> npm布局");
            //npm布局
            List<String> suffixList = Arrays.asList(".json", ".tgz");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof NugetFileSystem) {
            log.info("=====>>>>> nuget布局");
            //nuget布局
            List<String> suffixList = Arrays.asList(".nupkg", ".nuspec", ".config");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof PypiFileSystem) {
            log.info("=====>>>>> pypi布局");
            //pypi布局
            List<String> suffixList = Arrays.asList(".whl", ".egg", ".zip", ".gz");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        } else if (repositoryPath.getFileSystem() instanceof RawFileSystem) {
            log.info("=====>>>>> raw布局");
            //raw布局
            return true;
        } else if (repositoryPath.getFileSystem() instanceof RpmFileSystem) {
            log.info("=====>>>>> rpm布局");
            //rpm布局
            List<String> suffixList = Arrays.asList(".rpm");
            flag = endsWith(repositoryPath.getFileName().toString(), suffixList);
        }
        log.info("=====>>>>> 是否是该布局支持的类型：{}", flag);
        return flag;
    }
}
