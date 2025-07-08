package com.folib.scanner.analysis;

import com.folib.providers.io.RepositoryPath;

public interface BomAnalysis {

    /**
     * 分析bom CycloneDXJson
     * @param taskName 任务名称
     * @param filePath bom 文件路径
     */
    void  analysisCycloneDXJson(String taskName , RepositoryPath filePath);
}
