package com.veadan.folib.config;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import jakarta.inject.Inject;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
                    String newUrl = getUrls(urlList.get(0).toString());
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

    protected static String getRepositoryBaseUrl(Repository repository) {
        ConfigurationManager configurationManager = SpringUtil.getBean(ConfigurationManager.class);
        return String.format("%s/%s/%s", StringUtils.removeEnd(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

    public String getUrls(String url){
        String regex = "https?://[^/]+/(.*)";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(url);


        String regex2 = "local://(.+)";
        Pattern pattern2 = Pattern.compile(regex2);
        Matcher matcher2 = pattern2.matcher(url);

        if (matcher.find()) {
            return  matcher.group(1);
        }else if(matcher2.find()){
            return  matcher2.group(1);
        }else  {
            return url;
        }
    }

}
