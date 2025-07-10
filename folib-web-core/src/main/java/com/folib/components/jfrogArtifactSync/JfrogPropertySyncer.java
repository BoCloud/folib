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
package com.folib.components.jfrogArtifactSync;

import com.alibaba.fastjson.JSON;
import com.folib.domain.ArtifactMetadata;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author veadan
 * @since 2025-01-02 08:45
 */
public class JfrogPropertySyncer {

    private final Artifactory artifactory;

    private final Set<String> EXCLUDE_PROPERTIES=Set.of("sha256");

    public JfrogPropertySyncer(String url, String username, String apiKey){
        this.artifactory = ArtifactoryClientBuilder.create()
                .setUrl(url)
                .setUsername(username)
                .setPassword(apiKey)
                .build();
    }

    public String getPropertiesByKeyAndPath(String repoKey,String path){
        Map<String, List<String>> properties = this.artifactory.repository(repoKey).file(path).getProperties();
        HashMap<String, ArtifactMetadata> result = new HashMap<>();
        for (String key: properties.keySet()) {
            if(EXCLUDE_PROPERTIES.contains(key)){
                continue;
            }
            List<String> strings = properties.get(key);
            if(strings.size()==1){
                ArtifactMetadata artifactMetadata = ArtifactMetadata.builder().value(strings.get(0)).type("STRING").viewShow(1).build();
                result.put(key,artifactMetadata);
            }
        }
        if(result.isEmpty()){
            return null;
        }
        return JSON.toJSONString(result);

    }

    public void close(){
        if(this.artifactory!=null){
            this.artifactory.close();
        }
    }


}
