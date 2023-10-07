package com.veadan.folib.controllers.server;

import com.google.common.collect.Lists;
import com.veadan.folib.domain.FolderInfo;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.io.File;
import java.util.List;

/**
 * @author leipenghui
 * @date 2023/9/26
 **/
@PreAuthorize("hasAuthority('ADMIN')")
@RestController
@RequestMapping("/api/folder")
@Api(description = "目录", tags = "目录")
public class FolderController {

    /**
     * 获取目录列表
     *
     * @param directoryPath 目录名
     * @return 目录列表
     */
    @ApiOperation(value = "获取目录列表", response = FolderInfo.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping("/list")
    public ResponseEntity<List<FolderInfo>> getFoldersInDirectory(@RequestParam(required = false) String directoryPath) {
        if (directoryPath == null) {
            directoryPath = "/";
        }
        File directory = new File(directoryPath);
        if (!directory.exists() || !directory.isDirectory()) {
            throw new IllegalArgumentException("Invalid directory path");
        }
        List<FolderInfo> folderList = Lists.newArrayList();
        File[] subDirectories = directory.listFiles(File::isDirectory);
        if (subDirectories != null) {
            for (File subDirectory : subDirectories) {
                folderList.add(new FolderInfo(subDirectory.getName(), subDirectory.getAbsolutePath(), hasSubDirectories(subDirectory)));
            }
        }
        return ResponseEntity.ok(folderList);
    }

    /**
     * 判断是否有下级子目录
     *
     * @param directory 目录名
     * @return true 有子目录 false 无子目录
     */
    private boolean hasSubDirectories(File directory) {
        File[] subDirectories = directory.listFiles(File::isDirectory);
        return subDirectories != null && subDirectories.length > 0;
    }

}
