package com.veadan.folib.controllers.backup;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.forms.backup.BackupForm;
import com.veadan.folib.services.BackupService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;

/**
 * @author leipenghui
 * @date 2023/9/26
 **/
@PreAuthorize("hasAuthority('ADMIN')")
@RestController
@RequestMapping("/api/backup")
@Api(description = "备份策略", tags = "备份策略")
public class BackupController {

    @Inject
    private BackupService backupService;

    /**
     * 保存备份策略
     *
     * @param backupForm 备份策略参数
     * @return 结果
     */
    @ApiOperation(value = "保存备份策略")
    @AuditLog(value = AuditEventNameEnum.BACKUP_STRATEGY,target ="" )
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping
    public ResponseEntity<String> saveBackup(@RequestBody @Validated BackupForm backupForm) {
        backupService.saveBackup(backupForm);
        return ResponseEntity.ok("");
    }

}
