package com.veadan.folib.controllers.packagenameblock;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.domain.PackageNameBlockInfo;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.dto.packagenameblock.PackageNameBlockDto;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.PackageNameBlockService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;

/**
 * @author leipenghui
 */
@RestController
@RequestMapping("/api/packageNameBlock")
@Api(description = "包名阻断", tags = "包名阻断")
public class PackageNameBlockController extends BaseController {

    @Inject
    private PackageNameBlockService packageNameBlockService;

    @ApiOperation(value = "查询包名阻断分页列表", response = PackageNameBlockInfo.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_SECURITY_POLICY_CONFIGURATION')")
    @GetMapping(value = "/page")
    public TableResultResponse<PackageNameBlockInfo> page(@RequestParam(name = "page", required = false) Integer page,
                                                          @RequestParam(name = "limit", required = false) Integer limit,
                                                          PackageNameBlockDto packageNameBlockForm) {
        return packageNameBlockService.queryPackageNameBlockList(page, limit, packageNameBlockForm);
    }

    @ApiOperation(value = "查询包名阻断信息", response = PackageNameBlockInfo.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_SECURITY_POLICY_CONFIGURATION')")
    @GetMapping(value = "/info")
    public ResponseEntity<PackageNameBlockInfo> packageNameBlockInfo(PackageNameBlockDto packageNameBlockForm) {
        PackageNameBlockInfo packageNameBlockInfo = packageNameBlockService.selectOnePackageNameBlock(packageNameBlockForm);
        return ResponseEntity.ok(packageNameBlockInfo);
    }

    @ApiOperation(value = "保存包名阻断信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY, target = "'新增包名黑名单:' + #packageNameBlockForm.getPackageName()")
    @PutMapping
    public ResponseEntity<Void> savePackageNameBlock(@RequestBody PackageNameBlockDto packageNameBlockForm) {
        packageNameBlockService.savePackageNameBlock(packageNameBlockForm);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "更新包名阻断信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY, target = "'修改包名黑名单:' + #packageNameBlockForm.getPackageName()")
    @PostMapping
    public ResponseEntity<Void> updatePackageNameBlock(@RequestBody PackageNameBlockDto packageNameBlockForm) {
        packageNameBlockService.updatePackageNameBlock(packageNameBlockForm);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "删除包名阻断信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY, target = "'删除包名黑名单:' + #packageNameBlockForm.getPackageName()")
    @DeleteMapping
    public ResponseEntity<Void> deletePackageNameBlock(@RequestBody PackageNameBlockDto packageNameBlockForm) {
        packageNameBlockService.deletePackageNameBlock(packageNameBlockForm);
        return ResponseEntity.ok().build();
    }
}
