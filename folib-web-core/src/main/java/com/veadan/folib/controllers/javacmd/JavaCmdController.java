package com.veadan.folib.controllers.javacmd;

import com.veadan.folib.controllers.users.UserController;
import com.veadan.folib.services.JavaCmdService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.commons.CommonsMultipartFile;

import javax.inject.Inject;

@Controller
@RequestMapping("/api/cmdsevice")
@Api(description = "java 调用可执行exe文件",tags = "java 代码直接调用可执行exe文件")
@Slf4j
public class JavaCmdController {
    // 测试地址 http://localhost:38080/api/cmdsevice/getIndexFile?format=json&indexId=maven-local&chainId=1692595382056&url=http://10.10.33.149:8081/artifactory/maven-local
    @Inject
    JavaCmdService javaCmdService;

    @GetMapping("getIndexFile")
    @ApiOperation(value = "获取maven库索引文件")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Returns maven index details"),
            @ApiResponse(code = 403, message = "Unauthenticated access or user account has been disabled"),
            @ApiResponse(code = 404, message = UserController.NOT_FOUND_USER) })
    //@PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @ResponseBody
    public ResponseEntity getIndexFile(String format,String indexId,String chainId,String url){
        log.info("===========>获取maven仓库索引文件<================");
        return ResponseEntity.ok(javaCmdService.getArtifactIndex(format, indexId, chainId, url));
    }


    @PostMapping("uploadJsonFile")
    @ApiOperation(value = "获取maven库索引文件")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Returns maven index details"),
            @ApiResponse(code = 403, message = "Unauthenticated access or user account has been disabled"),
            @ApiResponse(code = 404, message = UserController.NOT_FOUND_USER) })
    //@PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @ResponseBody
    public ResponseEntity uploacJsonFile(@RequestParam(value="file") CommonsMultipartFile file,@RequestParam(value="baseUrl") String baseUrl){
        log.info("===========>获取maven仓库索引文件<================"+file.getSize());
        return ResponseEntity.ok(javaCmdService.parseFileAndDownLoad(file,baseUrl));
    }



}



