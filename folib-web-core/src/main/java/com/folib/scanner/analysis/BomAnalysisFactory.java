package com.folib.scanner.analysis;

import com.folib.providers.io.RepositoryPath;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Component
public class BomAnalysisFactory {

    @Resource
    private Map<String, BomAnalysis> bomAnalysisMap = new ConcurrentHashMap<>();

    public BomAnalysis getBomAnalysis(String type) {
        return bomAnalysisMap.get(type);
    }


    public void analysis(String taskName , RepositoryPath filePath){
        bomAnalysisMap.forEach((k,v)->{
                v.analysisCycloneDXJson(taskName,filePath);
        });
    }
}
