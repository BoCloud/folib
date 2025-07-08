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
