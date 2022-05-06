package com.veadan.folib.controllers.configuration;

import com.veadan.folib.forms.storage.routing.RoutingRuleForm;
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
 * @author Pablo Tirado
 * @author Przemyslaw Fusik
 */
@Controller
@RequestMapping("/api/configuration/folib/routing/rules")
@Api(value = "/api/configuration/folib/routing/rules")
public class RoutingConfigurationController
        extends BaseConfigurationController
{

    static final String SUCCESSFUL_ADD_ROUTING_RULE = "Successfully added routing rule.";

    static final String FAILED_ADD_ROUTING_RULE_FORM_ERRORS = "Routing rule cannot be added because the submitted form contains errors!";

    static final String FAILED_ADD_ROUTING_RULE = "Routing rule cannot be added.";

    static final String SUCCESSFUL_REMOVE_ROUTING_RULE = "Routing rule removed successfully.";

    static final String FAILED_REMOVE_ROUTING_RULE = "Routing rule cannot be removed.";

    static final String NOT_FOUND_REPOSITORY = "Routing rule could not be found.";

    static final String FAILED_UPDATE_ROUTING_RULE = "Successfully updated routing rule.";

    static final String FAILED_UPDATE_ROUTING_RULE_FORM_ERROR = "Routing rule cannot be updated because the submitted form contains errors!";

    private final ConversionService conversionService;

    public RoutingConfigurationController(ConfigurationManagementService configurationManagementService,
                                          ConversionService conversionService)
    {
        super(configurationManagementService);
        this.conversionService = conversionService;
    }

    @ApiOperation(value = "Returns routing rule for uuid.")
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

    @ApiOperation(value = "Returns routing rules.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Everything went ok.") })
    @GetMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getRoutingRules()
    {
        MutableRoutingRules body = configurationManagementService.getRoutingRules();
        return ResponseEntity.ok(body);
    }

    @ApiOperation(value = "Adds a routing rule.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_ADD_ROUTING_RULE),
                            @ApiResponse(code = 400, message = FAILED_ADD_ROUTING_RULE_FORM_ERRORS),
                            @ApiResponse(code = 404, message = FAILED_ADD_ROUTING_RULE) })
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity add(@RequestBody @Validated RoutingRuleForm routingRule,
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

    @ApiOperation(value = "Removes routing rule having provided uuid.")
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

    @ApiOperation(value = "Updates routing rule at the specified index.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = FAILED_UPDATE_ROUTING_RULE),
                            @ApiResponse(code = 400, message = FAILED_UPDATE_ROUTING_RULE_FORM_ERROR),
                            @ApiResponse(code = 404, message = NOT_FOUND_REPOSITORY) })
    @PutMapping(value = "/{uuid}",
                consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity update(@PathVariable UUID uuid,
                                 @RequestBody @Validated RoutingRuleForm routingRule,
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
