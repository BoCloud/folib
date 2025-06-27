package com.veadan.folib.controllers.backupstrategy;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.domain.backupstrategy.BackupStrategyRecord;
import com.veadan.folib.entity.BackupStrategy;
import com.veadan.folib.dto.backupstrategy.BackupStrategyDto;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.BackupStrategyService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.lang3.StringUtils;
import org.quartz.CronExpression;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.Objects;

/**
 * @author veadan
 */
@RestController
@RequestMapping("/api/backupStrategy")
@Api(description = "备份策略", tags = "备份策略")
public class BackupStrategyController extends BaseController {

    @Inject
    private BackupStrategyService backupStrategyService;

    @ApiOperation(value = "查询备份策略分页列表", response = BackupStrategyRecord.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/page")
    public TableResultResponse<BackupStrategyRecord> page(@RequestParam(name = "page", required = false) Integer page,
                                                          @RequestParam(name = "limit", required = false) Integer limit,
                                                          BackupStrategyDto backupStrategyForm) {
        return backupStrategyService.queryBackupStrategyPage(page, limit, backupStrategyForm);
    }

    @ApiOperation(value = "查询备份策略信息", response = BackupStrategyDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/info")
    public ResponseEntity<Object> backupStrategyInfo(BackupStrategy backupStrategy) {
        BackupStrategyDto backupStrategyForm = backupStrategyService.queryBackupStrategy(backupStrategy);
        if (Objects.isNull(backupStrategyForm)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("%s备份策略不存在", backupStrategy.getStrategyName()));
        }
        return ResponseEntity.ok(backupStrategyForm);
    }

    @ApiOperation(value = "保存备份策略信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PutMapping
    public ResponseEntity<String> saveBackupStrategy(@RequestBody @Validated(BackupStrategyDto.SaveGroup.class) BackupStrategyDto backupStrategyForm) {
        if (validateBackupStrategy(backupStrategyForm.getStrategyName())) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(String.format("%s备份策略名称已存在", backupStrategyForm.getStrategyName()));
        }
        ResponseEntity<String> responseEntity = validateCronExpression(backupStrategyForm.getCronExpression());
        if (Objects.nonNull(responseEntity)) {
            return responseEntity;
        }
        backupStrategyService.saveBackupStrategy(backupStrategyForm);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "修改备份策略信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping
    public ResponseEntity<String> updateBackupStrategy(@RequestBody @Validated(BackupStrategyDto.UpdateGroup.class) BackupStrategyDto backupStrategyForm) {
        if (Objects.isNull(backupStrategyForm.getId()) && !validateBackupStrategy(backupStrategyForm.getStrategyName())) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("%s备份策略不存在", backupStrategyForm.getStrategyName()));
        }
        ResponseEntity<String> responseEntity = validateCronExpression(backupStrategyForm.getCronExpression());
        if (Objects.nonNull(responseEntity)) {
            return responseEntity;
        }
        responseEntity = validateBackupStrategyName(backupStrategyForm.getId(), backupStrategyForm.getStrategyName());
        if (Objects.nonNull(responseEntity)) {
            return responseEntity;
        }
        backupStrategyService.updateBackupStrategy(backupStrategyForm);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "删除备份策略信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @DeleteMapping
    public ResponseEntity<String> deleteBackupStrategy(@RequestBody @Validated(BackupStrategyDto.DeleteGroup.class) BackupStrategyDto backupStrategyForm) {
        if (!validateBackupStrategy(backupStrategyForm.getStrategyName())) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("%s备份策略不存在", backupStrategyForm.getStrategyName()));
        }
        backupStrategyService.deleteBackupStrategy(BackupStrategy.builder().strategyName(backupStrategyForm.getStrategyName()).build());
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "执行备份策略")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping("/executeBackup")
    public ResponseEntity<String> executeBackup(@RequestBody @Validated(BackupStrategyDto.ExecuteGroup.class) BackupStrategyDto backupStrategyForm) {
        if (!validateBackupStrategy(backupStrategyForm.getStrategyName())) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("%s备份策略不存在", backupStrategyForm.getStrategyName()));
        }
        backupStrategyService.executeBackup(backupStrategyForm);
        return ResponseEntity.ok("ok");
    }

    private boolean validateBackupStrategy(String backupStrategyName) {
        BackupStrategy backupStrategy = backupStrategyService.getBackupStrategy(BackupStrategy.builder().strategyName(backupStrategyName).build());
        return Objects.nonNull(backupStrategy);
    }

    private ResponseEntity<String> validateBackupStrategyName(String id, String backupStrategyName) {
        if (Objects.isNull(id)) {
            return null;
        }
        BackupStrategy backupStrategy = backupStrategyService.getBackupStrategy(BackupStrategy.builder().id(Long.parseLong(id)).build());
        if (Objects.isNull(backupStrategy)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("%s备份策略不存在", backupStrategyName));
        }
        BackupStrategy existsBackupStrategy = backupStrategyService.getBackupStrategy(BackupStrategy.builder().strategyName(backupStrategyName).build());
        if (Objects.nonNull(existsBackupStrategy) && !existsBackupStrategy.getId().equals(backupStrategy.getId())) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("策略名称[%s]已被占用", backupStrategyName));
        }
        return null;
    }

    private ResponseEntity<String> validateCronExpression(String cronExpression) {
        if (StringUtils.isNotBlank(cronExpression) && !CronExpression.isValidExpression(cronExpression)) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(String.format("Cron表达式[%s]不正确，请检查", cronExpression));
        }
        return null;
    }


}
