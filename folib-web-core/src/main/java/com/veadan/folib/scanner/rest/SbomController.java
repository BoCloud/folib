package com.veadan.folib.scanner.rest;

import com.veadan.folib.scanner.analyze.AnalyzeService;
import com.veadan.folib.scanner.analyze.SbomAnalyzeServer;
import com.veadan.folib.scanner.task.AnalyzeSbomTask;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.cyclonedx.model.Bom;
import org.cyclonedx.parsers.JsonParser;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;


@Slf4j
@RestController
@RequestMapping(value = "/api/sca")
@Api(description = "sbom 分析", tags = "sbom")
public class SbomController {

    @Inject
    private SbomAnalyzeServer sbomAnalsisServer;
    @Inject
    private AnalyzeService analyzeService;


    @ApiOperation(value = " sbom cyclonedx json 分析", notes = "")
    @PostMapping(value = "/sbom/cyclonedx", consumes = "multipart/form-data", produces = "application/json")
    public ResponseEntity<?> analysisCyclonedx(
            @RequestPart(required = false) MultipartFile sbomFile,
            @RequestParam(required = true) int code,
            @RequestParam(required = false) String projectId,
            @RequestParam(required = true) String taskName,
            @RequestParam(required = false) String message
            ) throws Exception {
        Bom bom;
        boolean success;
        if ((code==200 && sbomFile != null)) {
            success = true;
            JsonParser jsonParser = new JsonParser();
            bom = jsonParser.parse(sbomFile.getInputStream());
        } else {
            bom = null;
            success = false;
        }
        AnalyzeSbomTask task = new AnalyzeSbomTask(1, taskName, () -> {
            try {
                sbomAnalsisServer.analyzeCycloneDx(bom, success, projectId, message, code, taskName);
            }catch (Exception e){
                log.error("sbom scanner error  code:{} projectId:{} taskName:{} message:{}",code, projectId, taskName, message);
                log.error("sbom scanner error:{}", ExceptionUtils.getStackTrace(e));
            }
        });
        analyzeService.addTask(task);
        return ResponseEntity.ok("susses");
    }


}
