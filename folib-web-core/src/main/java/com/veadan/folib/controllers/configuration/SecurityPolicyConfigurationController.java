package com.veadan.folib.controllers.configuration;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.ResponseMessage;
import com.veadan.folib.forms.configuration.SecurityPolicyConfigurationForm;
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
 * @author leipenghui
 */
@RestController
@RequestMapping("/api/configuration/folib/securityPolicy")
@Api(value = "/api/configuration/folib/securityPolicy")
public class SecurityPolicyConfigurationController extends BaseController {

    @Inject
    private SecurityPolicyConfigurationService securityPolicyConfigurationService;

    @ApiOperation(value = "设置平台级别白名单")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/setWhite")
    public ResponseEntity<ResponseMessage> setWhite(@RequestBody SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.setVulnerabilitiesWhites(securityPolicyConfigurationForm.getWhite());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "设置平台级别黑名单")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/setBlack")
    public ResponseEntity<ResponseMessage> setBlack(@RequestBody SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.setVulnerabilitiesBlacks(securityPolicyConfigurationForm.getBlack());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "添加平台级别白名单")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/addWhite")
    public ResponseEntity<ResponseMessage> addWhite(@RequestBody @Validated(SecurityPolicyConfigurationForm.WhiteGroup.class) SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.addVulnerabilitiesWhite(securityPolicyConfigurationForm.getWhite());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "添加平台级别黑名单")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/addBlack")
    public ResponseEntity<ResponseMessage> addBlack(@RequestBody @Validated(SecurityPolicyConfigurationForm.BlackGroup.class) SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.addVulnerabilitiesBlack(securityPolicyConfigurationForm.getBlack());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "删除平台级别白名单")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_DELETE_METADATA_CONFIGURATION')")
    @DeleteMapping(value = "/removeWhite")
    public ResponseEntity<ResponseMessage> removeWhite(@RequestBody @Validated(SecurityPolicyConfigurationForm.WhiteGroup.class) SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.removeVulnerabilitiesWhite(securityPolicyConfigurationForm.getWhite());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "删除平台级别黑名单")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_DELETE_METADATA_CONFIGURATION')")
    @DeleteMapping(value = "/removeBlack")
    public ResponseEntity<ResponseMessage> removeBlack(@RequestBody @Validated(SecurityPolicyConfigurationForm.BlackGroup.class) SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.removeVulnerabilitiesBlack(securityPolicyConfigurationForm.getBlack());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "保存或者更新平台通知配置")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/notify")
    public ResponseEntity<ResponseMessage> notify(@RequestBody SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.saveOrUpdateNotify(securityPolicyConfigurationForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "查询安全策略配置", response = SecurityPolicyConfigurationForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_SECURITY_POLICY_CONFIGURATION')")
    @GetMapping(value = "/config")
    public ResponseEntity<SecurityPolicyConfigurationForm> config() {
        return ResponseEntity.ok(securityPolicyConfigurationService.config());
    }

    @ApiOperation(value = "保存或者更新平台阻断配置")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping(value = "/block")
    public ResponseEntity<ResponseMessage> block(@RequestBody @Validated(SecurityPolicyConfigurationForm.BlockGroup.class) SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        securityPolicyConfigurationService.saveOrUpdateBlock(securityPolicyConfigurationForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }
}
