package com.veadan.folib.components.artifact;

import cn.hutool.core.io.FileUtil;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.UUID;

/**
 * @author leipenghui
 * @date 2022/12/15
 **/
@Component
public class ArtifactComponent {

    @Value("${folib.temp}")
    private String tempPath;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    /**
     * 读取文件内容
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param path         制品路径
     * @return 文件内容
     * @throws IOException io异常
     */
    public String readRepositoryPathContent(String storageId, String repositoryId, String path) throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        return readRepositoryPathContent(repositoryPath);
    }

    /**
     * 读取文件内容
     *
     * @param repositoryPath 路径
     * @return 文件内容
     * @throws IOException io异常
     */
    public String readRepositoryPathContent(RepositoryPath repositoryPath) throws IOException {
        String artifactContent = "";
        if (repositoryPath.getTarget() instanceof S3Path) {
            String parentPath = "";
            try {
                S3Path s3Path = (S3Path) repositoryPath.getTarget();
                InputStream inputStream = Files.newInputStream(repositoryPath);
                parentPath = tempPath + File.separator + UUID.randomUUID();
                String filePath = parentPath + File.separator + s3Path.getFileName();
                File tempFile = new File(filePath);
                FileUtil.writeFromStream(inputStream, tempFile, true);
                artifactContent = FileUtil.readString(tempFile, StandardCharsets.UTF_8);
            } catch (IOException ex) {
                throw new IOException(ex);
            } finally {
                //删除临时文件
                if (StringUtils.isNotBlank(parentPath)) {
                    FileUtil.del(new File(parentPath));
                }
            }
        } else {
            artifactContent = FileUtil.readString(repositoryPath.toAbsolutePath().toString(), StandardCharsets.UTF_8);
        }
        return artifactContent;
    }

}
