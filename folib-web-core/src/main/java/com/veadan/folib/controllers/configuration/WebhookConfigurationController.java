package com.veadan.folib.controllers.configuration;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.ResponseMessage;
import com.veadan.folib.entity.WebhookLog;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.forms.configuration.WebhookConfigurationForm;
import com.veadan.folib.services.WebhookService;
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
import java.util.List;

/**
 * @author veadan
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/configuration/folib/webhook")
@Api(description = "webhook管理",tags = "webhook管理")
public class WebhookConfigurationController extends BaseController {

    @Inject
    private WebhookService webhookService;

    @ApiOperation(value = "新增webhook")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @AuditLog(value = AuditEventNameEnum.WEB_HOOK,target =" '新增webhook url:'+#webhookConfigurationForm.url" )
    @PutMapping
    public ResponseEntity<ResponseMessage> addWebhookConfiguration(@RequestBody @Validated(WebhookConfigurationForm.AddGroup.class) WebhookConfigurationForm webhookConfigurationForm) throws IOException {
        webhookService.addWebhookConfiguration(webhookConfigurationForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "更新webhook")
    @AuditLog(value = AuditEventNameEnum.WEB_HOOK,target =" '更新webhook:'+#webhookConfigurationForm.uuid" )
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping
    public ResponseEntity<ResponseMessage> updateWebhookConfiguration(@RequestBody @Validated(WebhookConfigurationForm.UpdateGroup.class) WebhookConfigurationForm webhookConfigurationForm) throws IOException {
        webhookService.updateWebhookConfiguration(webhookConfigurationForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "删除webhook")
    @AuditLog(value = AuditEventNameEnum.WEB_HOOK,target ="'删除webhook:'+#webhookConfigurationForm.uuid" )
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @DeleteMapping
    public ResponseEntity<ResponseMessage> deleteWebhookConfiguration(@RequestBody @Validated(WebhookConfigurationForm.DeleteGroup.class) WebhookConfigurationForm webhookConfigurationForm) throws IOException {
        webhookService.deleteWebhookConfiguration(webhookConfigurationForm.getUuid());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "查询webhook")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping
    public ResponseEntity<List<WebhookConfigurationForm>> getWebhookConfiguration() throws IOException {
        return ResponseEntity.ok(webhookService.getWebhookConfiguration());
    }

    @ApiOperation(value = "查询webhookLog")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping("/log")
    public ResponseEntity<List<WebhookLog>> queryWebhookLog(WebhookLog webhookLog) throws IOException {
        return ResponseEntity.ok(webhookService.queryWebhookLogList(webhookLog));
    }

    @ApiOperation(value = "测试webhook")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PutMapping("/test")
    public ResponseEntity<Void> webhook(@RequestBody @Validated(WebhookConfigurationForm.TestGroup.class) WebhookConfigurationForm webhookConfigurationForm) throws IOException {
        webhookService.testWebhook(webhookConfigurationForm);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "删除webhookLog")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @DeleteMapping("/log")
    public ResponseEntity<Void> deleteWebhookLog(@RequestBody WebhookLog webhookLog) throws IOException {
        webhookService.deleteWebhookLog(webhookLog);
        return ResponseEntity.ok().build();
    }
}
