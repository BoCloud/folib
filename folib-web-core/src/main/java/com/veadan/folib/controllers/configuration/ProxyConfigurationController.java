package com.veadan.folib.controllers.configuration;

import com.veadan.folib.forms.configuration.ProxyConfigurationForm;
import com.veadan.folib.configuration.MutableProxyConfiguration;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.validation.RequestBodyValidationException;

import javax.validation.groups.Default;

import io.swagger.annotations.*;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * @author Pablo Tirado
 */
@Controller
@RequestMapping("/api/configuration/strongbox/proxy-configuration")
@Api(value = "/api/configuration/strongbox/proxy-configuration")
public class ProxyConfigurationController
        extends BaseConfigurationController
{

    static final String SUCCESSFUL_UPDATE = "The proxy configuration was updated successfully.";
    static final String FAILED_UPDATE_FORM_ERROR = "Proxy configuration cannot be updated because the submitted form contains errors!";
    static final String FAILED_UPDATE = "Failed to update the proxy configuration!";

    static final String NOT_FOUND_PROXY_CFG = "The proxy configuration for '${storageId}:${repositoryId}' was not found.";

    private final ConversionService conversionService;

    public ProxyConfigurationController(ConfigurationManagementService configurationManagementService,
                                        ConversionService conversionService)
    {
        super(configurationManagementService);
        this.conversionService = conversionService;
    }

    @ApiOperation(value = "Updates the proxy configuration for a repository, if one is specified, or, otherwise, the global proxy settings.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_UPDATE),
                            @ApiResponse(code = 400, message = FAILED_UPDATE_FORM_ERROR),
                            @ApiResponse(code = 500, message = FAILED_UPDATE) })
    @PreAuthorize("hasAuthority('CONFIGURATION_SET_GLOBAL_PROXY_CFG')")
    @PutMapping(value = "",
                consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity setProxyConfiguration(@ApiParam(value = "The storageId")
                                                @RequestParam(value = "storageId", required = false) String storageId,
                                                @ApiParam(value = "The repositoryId")
                                                @RequestParam(value = "repositoryId", required = false)
                                                        String repositoryId,
                                                @ApiParam(value = "The proxy configuration for this proxy repository", required = true)
                                                @RequestBody @Validated({ Default.class,
                                                                          ProxyConfigurationForm.ProxyConfigurationFormChecks.class })
                                                        ProxyConfigurationForm proxyConfigurationForm,
                                                BindingResult bindingResult,
                                                @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {

        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_UPDATE_FORM_ERROR, bindingResult);
        }

        MutableProxyConfiguration proxyConfiguration = conversionService.convert(proxyConfigurationForm,
                                                                                 MutableProxyConfiguration.class);
        logger.debug("Received proxy configuration\n: {}", proxyConfiguration);

        try
        {
            configurationManagementService.setProxyConfiguration(storageId, repositoryId, proxyConfiguration);
            return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE, acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_UPDATE, e, acceptHeader);
        }
    }

    @ApiOperation(value = "Returns the proxy configuration for a repository, if one is specified, or, otherwise, the global proxy settings.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = ""),
                            @ApiResponse(code = 404, message = NOT_FOUND_PROXY_CFG) })
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_GLOBAL_PROXY_CFG')")
    @GetMapping(value = "",
                produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getProxyConfiguration(@ApiParam(value = "The storageId")
                                                @RequestParam(value = "storageId", required = false) String storageId,
                                                @ApiParam(value = "The repositoryId")
                                                @RequestParam(value = "repositoryId", required = false)
                                                        String repositoryId,
                                                @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        MutableProxyConfiguration proxyConfiguration;
        if (storageId == null)
        {
            proxyConfiguration = configurationManagementService.getMutableConfigurationClone()
                                                               .getProxyConfiguration();
        }
        else
        {
            proxyConfiguration = configurationManagementService.getMutableConfigurationClone()
                                                               .getStorage(storageId)
                                                               .getRepository(repositoryId)
                                                               .getProxyConfiguration();
        }

        if (proxyConfiguration != null)
        {
            return ResponseEntity.ok(proxyConfiguration);
        }
        else
        {
            String message = "The proxy configuration" +
                             (storageId != null ? " for " + storageId + ":" + repositoryId : "") +
                             " was not found.";

            return getNotFoundResponseEntity(message, acceptHeader);
        }
    }
}
