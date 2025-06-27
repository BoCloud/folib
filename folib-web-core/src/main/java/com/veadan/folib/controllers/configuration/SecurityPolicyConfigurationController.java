package com.veadan.folib.controllers.configuration;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.ResponseMessage;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.dto.configuration.SecurityPolicyConfigurationDto;
import com.veadan.folib.services.SecurityPolicyConfigurationService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.io.IOException;

/**
 * @author veadan
 */
@RestController
@RequestMapping("/api/configuration/folib/securityPolicy")
@Api(description = "安全策略配置",tags = "安全策略配置")
public class SecurityPolicyConfigurationController extends BaseController {

    @Inject
    private SecurityPolicyConfigurationService securityPolicyConfigurationService;

    @ApiOperation(value = "设置平台级别白名单")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/setWhite")
    public ResponseEntity<ResponseMessage> setWhite(@RequestBody SecurityPolicyConfigurationDto securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.setVulnerabilitiesWhites(securityPolicyConfigurationForm.getWhite());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "设置平台级别黑名单")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/setBlack")
    public ResponseEntity<ResponseMessage> setBlack(@RequestBody SecurityPolicyConfigurationDto securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.setVulnerabilitiesBlacks(securityPolicyConfigurationForm.getBlack());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "添加平台级别白名单")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/addWhite")
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY,target =" '添加白名单:'+#securityPolicyConfigurationForm.white" )
    public ResponseEntity<ResponseMessage> addWhite(@RequestBody @Validated(SecurityPolicyConfigurationDto.WhiteGroup.class) SecurityPolicyConfigurationDto securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.addVulnerabilitiesWhite(securityPolicyConfigurationForm.getWhite());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "添加平台级别黑名单")
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY,target =" '添加黑名单:'+#securityPolicyConfigurationForm.black      " )
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/addBlack")
    public ResponseEntity<ResponseMessage> addBlack(@RequestBody @Validated(SecurityPolicyConfigurationDto.BlackGroup.class) SecurityPolicyConfigurationDto securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.addVulnerabilitiesBlack(securityPolicyConfigurationForm.getBlack());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "删除平台级别白名单")
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY,target =" '删除白名单:'+#securityPolicyConfigurationForm.white" )
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_DELETE_METADATA_CONFIGURATION')")
    @DeleteMapping(value = "/removeWhite")
    public ResponseEntity<ResponseMessage> removeWhite(@RequestBody @Validated(SecurityPolicyConfigurationDto.WhiteGroup.class) SecurityPolicyConfigurationDto securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.removeVulnerabilitiesWhite(securityPolicyConfigurationForm.getWhite());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "删除平台级别黑名单")
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY,target =" '删除黑名单:'+#securityPolicyConfigurationForm.white" )
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_DELETE_METADATA_CONFIGURATION')")
    @DeleteMapping(value = "/removeBlack")
    public ResponseEntity<ResponseMessage> removeBlack(@RequestBody @Validated(SecurityPolicyConfigurationDto.BlackGroup.class) SecurityPolicyConfigurationDto securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.removeVulnerabilitiesBlack(securityPolicyConfigurationForm.getBlack());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "保存或者更新平台通知配置")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/notify")
    public ResponseEntity<ResponseMessage> notify(@RequestBody SecurityPolicyConfigurationDto securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.saveOrUpdateNotify(securityPolicyConfigurationForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "查询安全策略配置", response = SecurityPolicyConfigurationDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_SECURITY_POLICY_CONFIGURATION')")
    @GetMapping(value = "/config")
    public ResponseEntity<SecurityPolicyConfigurationDto> config() {
        return ResponseEntity.ok(securityPolicyConfigurationService.config());
    }

    @ApiOperation(value = "保存或者更新平台阻断配置")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/block")
    public ResponseEntity<ResponseMessage> block(@RequestBody @Validated(SecurityPolicyConfigurationDto.BlockGroup.class) SecurityPolicyConfigurationDto securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.saveOrUpdateBlock(securityPolicyConfigurationForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "新增包名阻断配置")
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY,target =" '新增阻断包:'+#securityPolicyConfigurationForm.packageNames" )
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/packageName")
    public ResponseEntity<ResponseMessage> addPackageName(@RequestBody @Validated(SecurityPolicyConfigurationDto.BlockGroup.class) SecurityPolicyConfigurationDto securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.addPackageName(securityPolicyConfigurationForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "删除包名阻断配置")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY,target ="'删除阻断包:'+#securityPolicyConfigurationForm.packageNames" )
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @DeleteMapping(value = "/packageName")
    public ResponseEntity<ResponseMessage> deletePackageName(@RequestBody @Validated(SecurityPolicyConfigurationDto.BlockGroup.class) SecurityPolicyConfigurationDto securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.deletePackageName(securityPolicyConfigurationForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }
}
