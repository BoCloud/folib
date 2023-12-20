package com.veadan.folib.controllers;

import com.veadan.folib.model.request.ArtifactSyncSlaveRecordAddReq;
import com.veadan.folib.model.request.ArtifactSyncSlaveRecordUpdateReq;
import com.veadan.folib.model.response.Result;
import com.veadan.folib.services.ArtifactSyncSlaveRecordService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;
import java.util.List;
import java.util.Map;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/20 14:16
 * @since x.x.x
 */
@RestController
@RequestMapping("/api/artifactSyncSlaveRecord")
@Api(value = "制品晋级从记录")
public class ArtifactSyncSlaveRecordController {
    @Autowired
    private ArtifactSyncSlaveRecordService artifactSyncSlaveRecordService;

    @ApiOperation(value = "批量添加制品晋级/分发从记录")
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @PostMapping(value = "/batch")
    public Result<Map<String, Long>> batchAdd(@RequestBody @Validated List<ArtifactSyncSlaveRecordAddReq> models) {
        return Result.success(artifactSyncSlaveRecordService.batchAdd(models));
    }

    @ApiOperation(value = "更新制品晋级/分发从记录")
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @PutMapping
    public Result<Boolean> update(@RequestBody @Validated ArtifactSyncSlaveRecordUpdateReq model) {
        return Result.success(artifactSyncSlaveRecordService.update(model));
    }


    @ApiOperation(value = "批量更新制品晋级/分发从记录")
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @PutMapping(value = "/batch")
    public Result<Boolean> batchUpdate(List<ArtifactSyncSlaveRecordUpdateReq> models) {
        return Result.success(artifactSyncSlaveRecordService.batchUpdate(models));
    }

}
