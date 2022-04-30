package com.veadan.folib.controllers.configuration.security.cors;

import com.veadan.folib.forms.configuration.CorsConfigurationForm;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.services.ConfigurationManagementService;

import javax.inject.Inject;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

/**
 * @author Przemyslaw Fusik
 * @author Pablo Tirado
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping(value = "/api/configuration/cors")
@Api(value = "/api/configuration/cors")
public class CorsConfigurationController
        extends BaseController
{
    public static final String SUCCESSFUL_UPDATE = "CORS allowed origins was updated.";

    public static final String FAILED_UPDATE = "Could not update CORS allowed origins.";

    private static final Logger logger = LoggerFactory.getLogger(CorsConfigurationController.class);

    @Inject
    private CorsConfigurationSource corsConfigurationSource;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @ApiOperation(value = "Returns allowed origins")
    @ApiResponses(value = @ApiResponse(code = 200, message = "Allowed origins."))
    @GetMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getAllowedOrigins()
    {
        List<String> allowedOrigins = ((UrlBasedCorsConfigurationSource) corsConfigurationSource).getCorsConfigurations()
                                                                                                 .values()
                                                                                                 .stream()
                                                                                                 .flatMap(
                                                                                                         c -> c.getAllowedOrigins() !=
                                                                                                              null ?
                                                                                                              c.getAllowedOrigins().stream() :
                                                                                                              Stream.empty())
                                                                                                 .collect(
                                                                                                         Collectors.toList());
        return getJSONListResponseEntityBody("origins", allowedOrigins);
    }

    @ApiOperation(value = "Sets CORS allowed origins",
                  notes = "In the request body, put an array of all allowed origins like this example: " +
                          "allowedOrigins: [\"http://example-a.com/\",\"http://example-b.com/foo/bar\"] " +
                          "You can always provide [\"*\"] to allow all origins.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = CorsConfigurationController.SUCCESSFUL_UPDATE),
                            @ApiResponse(code = 400, message = CorsConfigurationController.FAILED_UPDATE) })
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity setAllowedOrigins(@RequestBody @Validated CorsConfigurationForm corsSettingsForm,
                                            BindingResult bindingResult,
                                            @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        Optional<CorsConfiguration> corsConfiguration = ((UrlBasedCorsConfigurationSource) corsConfigurationSource).getCorsConfigurations()
                                                                                                                   .values()
                                                                                                                   .stream()
                                                                                                                   .findFirst();

        if (corsConfiguration.isPresent())
        {
            if (setAllowedOrigins(corsConfiguration.get(), corsSettingsForm.getAllowedOrigins()))
            {
                return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE, acceptHeader);
            }
        }

        return getBadRequestResponseEntity(FAILED_UPDATE, acceptHeader);
    }

    private boolean setAllowedOrigins(CorsConfiguration corsConfiguration,
                                      List<String> allowedOrigins)
    {
        try
        {
            configurationManagementService.setCorsAllowedOrigins(allowedOrigins);
        }
        catch (Exception ex)
        {
            logger.error("Unable to set CORS allowed origins", ex);
            return false;
        }

        corsConfiguration.setAllowedOrigins(allowedOrigins);

        return true;
    }

}
