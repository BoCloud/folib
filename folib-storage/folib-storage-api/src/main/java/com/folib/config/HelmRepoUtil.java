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
package com.folib.config;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.configuration.ConfigurationManager;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactResolutionService;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import jakarta.inject.Inject;
import java.io.FileInputStream;
import java.io.FileWriter;
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
 * @author veadan
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
