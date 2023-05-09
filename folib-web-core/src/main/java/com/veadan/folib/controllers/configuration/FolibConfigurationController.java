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
 * @author Veadan
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

    @ApiOperation(value = "上传 folib.yaml 并重新加载服务器的配置.")
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

        logger.info("通过 REST 接收新配置");

        return new ResponseEntity<>(ResponseMessage.empty().withMessage("配置更新成功."),
                                    HttpStatus.OK);
    }

    @ApiOperation(value = "检索 folib.yaml 配置文件。")
    @ApiResponses(value = { @ApiResponse(code = 200, message = ""),
                            @ApiResponse(code = 500, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW')")
    @GetMapping(produces = { com.veadan.folib.net.MediaType.APPLICATION_YAML_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity<MutableConfiguration> getFolibConfiguration()
    {
        logger.info("Retrieved folib.yaml configuration file.");

        return new ResponseEntity<>(getMutableConfigurationClone(), HttpStatus.OK);
    }

}
