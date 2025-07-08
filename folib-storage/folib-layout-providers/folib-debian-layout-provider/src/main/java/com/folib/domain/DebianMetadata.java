package com.folib.domain;

import lombok.Data;
import lombok.NoArgsConstructor;

import javax.annotation.Nonnull;
import java.io.Serializable;
import java.util.List;

/**
 * @author veadan
 **/
@Data
@NoArgsConstructor
public class DebianMetadata implements Serializable, Comparable<DebianMetadata> {

    public String packageName;

    /**
     * 包的版本
     */
    public String version;

    /**
     * 包的目标架构（如 amd64, i386, all）
     */
    public String architecture;

    /**
     * 包的维护者信息
     */
    public String maintainer;


    /**
     * 该包依赖的其他包列表
     */
    public List<String> depends;

    /**
     * 包所属的类别（如 utils, net, admin）
     */
    public String section;

    /**
     * 包的优先级（required, important, standard, optional, extra）
     */
    public String priority;

    /**
     * 包的官方网站
     */
    public String homePage;

    public String website;

    /**
     * 包的详细描述
     */
    public String description;

    /**
     * 包文件在仓库中的路径
     */
    public String filename;


    /**
     * 包文件的 MD5 校验和
     */
    public String md5sum;

    /**
     * 包文件的 SHA1 校验和
     */
    public String sha1;

    /**
     * 包文件的 SHA256 校验和
     */
    public String sha256;

    public String size;

    public String controlFileContent;

    public String license;

    public String source;

    public String artifactRelativePath;


    /**
     * 构造函数
     *
     * @param packageName  包名
     * @param version      版本
     * @param architecture 架构
     */
    public DebianMetadata(String packageName, String version, String architecture) {
        this.packageName = packageName;
        this.version = version;
        this.architecture = architecture;
    }

    /**
     * 添加一个依赖项
     *
     * @param dependency 依赖项
     */
    public void addDepends(String dependency) {
        this.depends.add(dependency);
    }

    public int compareTo(@Nonnull DebianMetadata o) {
        return (this.packageName + "-" + this.architecture + "-" + this.version).compareTo(o.packageName + "-" + o.architecture + "-" + o.version);
    }


}
