package com.veadan.folib.controllers.configuration;

import com.veadan.folib.configuration.MutableConfiguration;
import com.veadan.folib.controllers.ResponseMessage;
import com.veadan.folib.services.ConfigurationManagementService;

import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import java.io.IOException;

/**
 * @author Pablo Tirado
 */
@Controller
@RequestMapping("/api/configuration/folib")
@Api(value = "/api/configuration/folib")
public class FolibConfigurationController
        extends BaseConfigurationController
{

    public FolibConfigurationController(ConfigurationManagementService configurationManagementService)
    {
        super(configurationManagementService);
    }

    @ApiOperation(value = "Upload a folib.yaml and reload the server's configuration.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The configuration was updated successfully."),
                            @ApiResponse(code = 500, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('CONFIGURATION_UPLOAD')")
    @PutMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE,
                             com.veadan.folib.net.MediaType.APPLICATION_YAML_VALUE },
            consumes = { MediaType.APPLICATION_JSON_VALUE,
                         com.veadan.folib.net.MediaType.APPLICATION_YAML_VALUE })
    public ResponseEntity<ResponseMessage> setFolibConfiguration(
            @ApiParam(value = "The folib.yaml configuration file", required = true) @RequestBody
                    MutableConfiguration configuration) throws IOException
    {
        configurationManagementService.setConfiguration(configuration);

        logger.info("Received new configuration over REST.");

        return new ResponseEntity<>(ResponseMessage.empty().withMessage("The configuration was updated successfully."),
                                    HttpStatus.OK);
    }

    @ApiOperation(value = "Retrieves the folib.yaml configuration file.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = ""),
                            @ApiResponse(code = 500, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW')")
    @GetMapping(produces = { com.veadan.folib.net.MediaType.APPLICATION_YAML_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity<MutableConfiguration> getFolibConfiguration()
    {
        logger.debug("Retrieved folib.yaml configuration file.");

        return new ResponseEntity<>(getMutableConfigurationClone(), HttpStatus.OK);
    }

}
