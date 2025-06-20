package com.veadan.folib.controllers.configuration;

import com.veadan.folib.dto.storage.routing.RoutingRuleDto;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.routing.MutableRoutingRule;
import com.veadan.folib.storage.routing.MutableRoutingRules;
import com.veadan.folib.validation.RequestBodyValidationException;

import java.io.IOException;
import java.util.UUID;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * @author Veadan
 * @author veadan
 */
@Controller
@RequestMapping("/api/configuration/folib/routing/rules")
@Api(description = "路由规则管理",tags = "路由规则管理")
public class RoutingConfigurationController
        extends BaseConfigurationController
{

    static final String SUCCESSFUL_ADD_ROUTING_RULE = "成功添加路由规则.";

    static final String FAILED_ADD_ROUTING_RULE_FORM_ERRORS = "提交的表单包含错误，无法添加路由规则!";

    static final String FAILED_ADD_ROUTING_RULE = "无法添加路由规则.";

    static final String SUCCESSFUL_REMOVE_ROUTING_RULE = "路由规则删除成功.";

    static final String FAILED_REMOVE_ROUTING_RULE = "无法删除路由规则.";

    static final String NOT_FOUND_REPOSITORY = "找不到路由规则.";

    static final String FAILED_UPDATE_ROUTING_RULE = "成功更新路由规则.";

    static final String FAILED_UPDATE_ROUTING_RULE_FORM_ERROR = "无法更新路由规则，因为提交的表单包含错误!";

    private final ConversionService conversionService;

    public RoutingConfigurationController(ConfigurationManagementService configurationManagementService,
                                          ConversionService conversionService)
    {
        super(configurationManagementService);
        this.conversionService = conversionService;
    }

    @ApiOperation(value = "返回 uuid 的路由规则.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Everything went ok."),
                            @ApiResponse(code = 404, message = NOT_FOUND_REPOSITORY) })
    @GetMapping(value = "{uuid}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getRoutingRule(@PathVariable UUID uuid,
                                         @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        MutableRoutingRule body = configurationManagementService.getRoutingRule(uuid);

        if (body == null)
        {
            return getNotFoundResponseEntity(NOT_FOUND_REPOSITORY, accept);
        }

        return ResponseEntity.ok(body);
    }

    @ApiOperation(value = "返回路由规则.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Everything went ok.") })
    @GetMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getRoutingRules()
    {
        MutableRoutingRules body = configurationManagementService.getRoutingRules();
        return ResponseEntity.ok(body);
    }

    @ApiOperation(value = "添加路由规则.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_ADD_ROUTING_RULE),
                            @ApiResponse(code = 400, message = FAILED_ADD_ROUTING_RULE_FORM_ERRORS),
                            @ApiResponse(code = 404, message = FAILED_ADD_ROUTING_RULE) })
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity add(@RequestBody @Validated RoutingRuleDto routingRule,
                              BindingResult bindingResult,
                              @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_ADD_ROUTING_RULE_FORM_ERRORS, bindingResult);
        }

        MutableRoutingRule rule = conversionService.convert(routingRule, MutableRoutingRule.class);
        final boolean added = configurationManagementService.addRoutingRule(rule);

        return getResponse(added, SUCCESSFUL_ADD_ROUTING_RULE, FAILED_ADD_ROUTING_RULE, acceptHeader);
    }

    @ApiOperation(value = "删除提供了 uuid 的路由规则.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_REMOVE_ROUTING_RULE),
                            @ApiResponse(code = 404, message = FAILED_ADD_ROUTING_RULE) })
    @DeleteMapping(value = "/{uuid}",
                   produces = { MediaType.TEXT_PLAIN_VALUE,
                                MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity remove(@PathVariable UUID uuid,
                                 @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        final boolean removed = configurationManagementService.removeRoutingRule(uuid);

        return getResponse(removed, SUCCESSFUL_REMOVE_ROUTING_RULE, FAILED_REMOVE_ROUTING_RULE, acceptHeader);
    }

    @ApiOperation(value = "更新指定索引处的路由规则.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = FAILED_UPDATE_ROUTING_RULE),
                            @ApiResponse(code = 400, message = FAILED_UPDATE_ROUTING_RULE_FORM_ERROR),
                            @ApiResponse(code = 404, message = NOT_FOUND_REPOSITORY) })
    @PutMapping(value = "/{uuid}",
                consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity update(@PathVariable UUID uuid,
                                 @RequestBody @Validated RoutingRuleDto routingRule,
                                 BindingResult bindingResult,
                                 @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_UPDATE_ROUTING_RULE_FORM_ERROR, bindingResult);
        }

        MutableRoutingRule rule = conversionService.convert(routingRule, MutableRoutingRule.class);

        final boolean updated = configurationManagementService.updateRoutingRule(uuid, rule);

        return getResponse(updated, FAILED_UPDATE_ROUTING_RULE, FAILED_UPDATE_ROUTING_RULE, acceptHeader);
    }

    private ResponseEntity getResponse(boolean result,
                                       String successfulMessage,
                                       String notFoundMessage,
                                       String acceptHeader)
    {
        if (result)
        {
            return getSuccessfulResponseEntity(successfulMessage, acceptHeader);
        }
        else
        {
            return getNotFoundResponseEntity(notFoundMessage, acceptHeader);
        }
    }

}
