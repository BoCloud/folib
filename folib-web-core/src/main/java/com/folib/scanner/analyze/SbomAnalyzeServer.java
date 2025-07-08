package com.folib.scanner.analyze;

import org.cyclonedx.exception.GeneratorException;
import org.cyclonedx.model.Bom;

import java.io.IOException;

public interface SbomAnalyzeServer {

    void analyzeCycloneDxBom( Bom bom) throws Exception;
    /**
     * 分析sbom
     * @param bom sbom
     */
    void analyzeCycloneDx(Bom bom,Boolean isAnalysis) throws GeneratorException, IOException;

    void analyzeCycloneDx(Bom bom, boolean success,String projectId, String message,int priority, String taskName) throws Exception;
}
