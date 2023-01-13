package com.veadan.folib.config;

import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import javax.inject.Inject;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * helm 仓库工具类
 *
 * @author qijianping
 */
@Component
@Slf4j
public class HelmRepoUtil {

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    private final static DumperOptions OPTIONS = new DumperOptions();

    static {
        //设置yaml读取方式为块读取
        OPTIONS.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
        OPTIONS.setDefaultScalarStyle(DumperOptions.ScalarStyle.PLAIN);
        OPTIONS.setPrettyFlow(false);
    }

    /**
     * 创建与更新Helm 仓库 索引文件
     *
     * @param fileAbsolutePath 仓库路径
     * @throws IOException          io 异常
     * @throws InterruptedException 中断的异常
     */
    public void createIndex(String fileAbsolutePath, Repository repository) throws IOException, InterruptedException {
        Runtime runtime = Runtime.getRuntime();
        StringBuilder helmCommand = new StringBuilder();
        helmCommand
                .append("helm repo index ")
                .append(fileAbsolutePath)
                .append(" --url ")
                .append(getRepositoryHostUrl(repository));
        String command = helmCommand.toString();
        Process pro = runtime.exec(command);
        int status = pro.waitFor();
        if (status != 0) {
            log.error("Failed to call shell's command ");
        }
//        RepositoryPath path = repositoryPathResolver.resolve(repository,"index.yaml");
//        Files.setAttribute(path, RepositoryFiles.formatAttributes(RepositoryFileAttributeType.METADATA),"index.yaml");

    }

    public void reloadIndex(RepositoryPath repositoryPath) {
        String fileName = repositoryPath.getTarget().toString();
        LinkedHashMap<String, Object> yamls = new LinkedHashMap<>();
        Yaml yaml = new Yaml(OPTIONS);
        try (InputStream inputStream = new FileInputStream(fileName)) {
            yamls = yaml.loadAs(inputStream, LinkedHashMap.class);
            Map charts = (Map) yamls.get("entries");
            charts.forEach((x, y) -> {
                List chartList = (List) y;
                chartList.forEach(j -> {
                    Map chartMap = (Map) j;
                    List urlList = (ArrayList) chartMap.get("urls");
                    String url = urlList.get(0).toString();
                    String newUrl = getRepositoryHostUrl(repositoryPath.getRepository()) + "/" + url.split("/")[url.split("/").length - 1];
                    urlList.clear();
                    urlList.add(newUrl);
                });
            });
            yaml.dump(yamls, new FileWriter(fileName));
//            RepositoryPath path = repositoryPathResolver.resolve(repositoryPath.getRepository(),"index.yaml");
//            Files.setAttribute(path, RepositoryFiles.formatAttributes(RepositoryFileAttributeType.METADATA),"index.yaml");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private String getRepositoryHostUrl(Repository repository) {
        StringBuilder urlBuilder = new StringBuilder();
        urlBuilder.append("http://")
                .append(System.getProperty("folib.host") != null ? System.getProperty("folib.host") : "localhost")
                .append(":")
                .append(System.getProperty("folib.port") != null ?
                        Integer.parseInt(System.getProperty("folib.port")) : 38080).append("/")
                .append(repository.getStorage().getId())
                .append("/")
                .append(repository.getId());
        return urlBuilder.toString();

    }

}
