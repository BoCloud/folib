package com.veadan.folib.components.sbom;

import cn.hutool.core.io.FileUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Sets;
import com.veadan.folib.enums.SBOMBinTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermission;
import java.util.*;

/**
 * @author leipenghui
 * @date 2024/10/30
 **/
@Slf4j
@Component
public class SbomComponent {

    @Value("${folib.temp}")
    private String tempPath;

    /**
     * 获取sbom信息
     *
     * @param path 路径
     * @return sbom信息
     */
    public JSONObject getSbom(Path path) {
        String targetPath = "", binPath = "", fileName = FilenameUtils.getBaseName(path.getFileName().toString());
        JSONObject jsonData = null;
        try {
            long startTime = System.currentTimeMillis();
            log.info("获取SBOM [{}] 开始", fileName);
            binPath = getBinPath();
            File binFile = new File(binPath);
            targetPath = binFile.getParent() + File.separator + UUID.randomUUID().toString() + File.separator + fileName + ".json";
            File targetFile = new File(targetPath);
            String command = binFile.getAbsolutePath() + " " + path.toAbsolutePath().toString() + " -o cyclonedx-json=" + targetFile.getAbsolutePath();
            log.info("Command [{}]", command);
            Process process = Runtime.getRuntime().exec(command);
            //等待命令执行完成
            process.waitFor();
            log.info("获取SBOM [{}] 结束 耗时约为 [{}] 毫秒", fileName, System.currentTimeMillis() - startTime);
            if (targetFile.exists()) {
                String data = Files.readString(Path.of(targetFile.getAbsolutePath()));
                if (StringUtils.isNotBlank(data) && JSONUtil.isJson(data)) {
                    jsonData = JSONObject.parseObject(data);
                }
            }
            return jsonData;
        } catch (Exception e) {
            log.error("获取SBOM [{}] 错误 [{}]", fileName, ExceptionUtils.getStackTrace(e));
        } finally {
            if (StringUtils.isNotBlank(targetPath)) {
                try {
                    FileUtil.del(Path.of(targetPath).getParent());
                } catch (Exception ex) {
                    log.error("删除 [{}] 临时文件错误 [{}]", targetPath, ExceptionUtils.getStackTrace(ex));
                }
            }
        }
        return null;
    }

    /**
     * 获取sbom组件信息
     *
     * @param path 路径
     * @return sbom组件信息
     */
    public List<BomComponent> sbomComponent(Path path) {
        try {
            JSONObject dataJson = getSbom(path);
            if (Objects.isNull(dataJson)) {
                return null;
            }
            String componentsKey = "components";
            if (!dataJson.containsKey(componentsKey)) {
                return null;
            }
            String componentsValue = dataJson.getString(componentsKey);
            if (StringUtils.isBlank(componentsValue) || !JSONUtil.isJsonArray(componentsValue)) {
                return null;
            }
            List<BomComponent> bomComponents = JSONArray.parseArray(componentsValue, BomComponent.class);
            String sha1Key = "SHA-1";
            if (CollectionUtils.isNotEmpty(bomComponents)) {
                bomComponents.forEach(item -> {
                    if (CollectionUtils.isNotEmpty(item.getExternalReferences())) {
                        item.getExternalReferences().forEach(e -> {
                            if (CollectionUtils.isNotEmpty(e.getHashes())) {
                                Optional<BomHash> optionalBomHash = e.getHashes().stream().filter(h -> sha1Key.equalsIgnoreCase(h.getAlg())).findFirst();
                                optionalBomHash.ifPresent(bomHash -> item.setSha1(bomHash.getContent()));
                            }
                        });
                        item.setExternalReferences(null);
                    }
                });
            }
            return bomComponents;
        } catch (Exception ex) {
            log.error("获取SBOM [{}] 错误 [{}]", path.toString(), ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    /**
     * 获取解析SBOM的脚本路径
     *
     * @return 脚本路径
     */
    private String getBinPath() {
        try {
            String path = SBOMBinTypeEnum.UNIX_AMD.getPath();
            log.info("IS_OS_LINUX [{}] IS_OS_MAC [{}] IS_OS_WINDOWS [{}] cpuArch [{}]", SystemUtils.IS_OS_LINUX, SystemUtils.IS_OS_MAC, SystemUtils.IS_OS_WINDOWS, SystemUtils.OS_ARCH);
            String x86 = "x86", amd = "amd", arm = "arm";
            if (SystemUtils.IS_OS_LINUX) {
                String cpuArch = SystemUtils.OS_ARCH;
                if (cpuArch.contains(x86) || cpuArch.contains(amd)) {
                    path = SBOMBinTypeEnum.UNIX_AMD.getPath();
                } else if (cpuArch.contains(arm)) {
                    path = SBOMBinTypeEnum.UNIX_ARM.getPath();
                }
            } else if (SystemUtils.IS_OS_MAC) {
                String cpuArch = SystemUtils.OS_ARCH;
                if (cpuArch.contains(x86)) {
                    path = SBOMBinTypeEnum.MAC_AMD.getPath();
                } else if (cpuArch.contains(arm)) {
                    path = SBOMBinTypeEnum.MAC_ARM.getPath();
                }
            } else if (SystemUtils.IS_OS_WINDOWS) {
                path = SBOMBinTypeEnum.WINDOWS.getPath();
            }
            ClassLoader classLoader = getClass().getClassLoader();
            String targetPath = tempPath + File.separator + "sbomBin" + File.separator + path.substring(path.lastIndexOf("/") + 1);
            Path binPath = Path.of(targetPath);
            if (!Files.exists(binPath)) {
                try (InputStream inputStream = classLoader.getResourceAsStream(path)) {
                    if (Objects.nonNull(inputStream)) {
                        Path parentDir = binPath.getParent();
                        if (parentDir != null && !Files.exists(parentDir)) {
                            Files.createDirectories(parentDir);
                        }
                        // 创建目标文件（如果不存在）
                        if (!Files.exists(binPath)) {
                            Files.createFile(binPath);
                        }
                        Files.copy(inputStream, binPath, StandardCopyOption.REPLACE_EXISTING);
                        // 设置目标文件的权限（示例中使用 POSIX 权限）
                        Set<PosixFilePermission> permissions = Sets.newHashSet();
                        permissions.add(PosixFilePermission.OWNER_READ);
                        permissions.add(PosixFilePermission.OWNER_WRITE);
                        permissions.add(PosixFilePermission.OWNER_EXECUTE);
                        Files.setPosixFilePermissions(binPath, permissions);
                    }
                }
            }
            log.info("获取SBOM的脚本path [{}]", targetPath);
            return targetPath;
        } catch (Exception ex) {
            log.error("获取SBOM的脚本错误 [{}]", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("获取SBOM的脚本");
        }
    }
}
