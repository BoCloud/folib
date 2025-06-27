package com.veadan.folib.domain;

import com.alibaba.fastjson.annotation.JSONField;
import com.github.zafarkhaja.semver.Version;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author veadan
 * @date 2024/6/13
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PubPackageVersionMetadata implements Serializable, Comparable<PubPackageVersionMetadata> {

    /**
     * 版本号。
     */
    private String version;

    /**
     * 是否被撤回。
     */
    private Boolean retracted;

    /**
     * 发布时间。
     */
    private String published;

    /**
     * 包配置信息
     */
    private Pubspec pubspec;

    /**
     * 制品包下载URL。
     * https://pub.dev/api/archives/meta-0.2.7.tar.gz
     * http://192.168.5.116:8081/artifactory/api/pub/pub-remote/packages/js/versions/0.0.26.tar.gz
     */
    @JSONField(name = "archive_url")
    private String archiveUrl;

    /**
     * 源制品包下载URL。
     */
    private String sourceArchiveUrl;

    /**
     * 制品包SHA256值。
     */
    @JSONField(name = "archive_sha256")
    private String archiveSha256;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        PubPackageVersionMetadata that = (PubPackageVersionMetadata) o;
        return version.equals(that.version);
    }

    @Override
    public int hashCode() {
        return Objects.hash(version);
    }

    @Override
    public int compareTo(@NotNull PubPackageVersionMetadata o) {
        return Version.valueOf(getVersion()).compareWithBuildsTo(Version.valueOf(o.getVersion()));
    }
}
