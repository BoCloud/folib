package com.veadan.folib.controllers.configuration.security.authentication;

import javax.inject.Inject;

import com.veadan.folib.authentication.ConfigurableProviderManager;
import com.veadan.folib.authentication.api.AuthenticationItems;
import com.veadan.folib.controllers.BaseController;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * @author Przemyslaw Fusik
 * @author Pablo Tirado
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/configuration/authenticators")
@Api(value = "/api/configuration/authenticators")
public class AuthenticatorsConfigController
        extends BaseController
{

    public static final String SUCCESSFUL_UPDATE = "Update succeeded.";
    
    public static final String SUCCESSFUL_REORDER = "Re-order succeeded.";

    public static final String FAILED_REORDER = "Could not reorder authentication items.";
    
    public static final String FAILED_UPDATE= "Could not update authentication configuration.";

    public static final String SUCCESSFUL_RELOAD = "Authentication configuration re-load succeeded.";

    public static final String FAILED_RELOAD = "Could not reload authentication configuration.";

    @Inject
    private ConfigurableProviderManager providerManager;

    @ApiOperation(value = "Enumerates ordered collection of authenticators with order number and name")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The list was returned successfully.") })
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public AuthenticationItems list()
    {
        return providerManager.getAuthenticationItems();
    }

    @ApiOperation(value = "Reorders authenticators by their names")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_REORDER),
                            @ApiResponse(code = 400, message = FAILED_REORDER) })
    @PutMapping(path = "/reorder/{first}/{second}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity reorder(@PathVariable String first,
                                  @PathVariable String second,
                                  @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        try
        {
            providerManager.reorder(first, second);
            return getSuccessfulResponseEntity(SUCCESSFUL_REORDER, acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.BAD_REQUEST, FAILED_REORDER, e, acceptHeader);
        }
    }

    @ApiOperation(value = "Reorders authenticators by their indexes")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_REORDER),
                            @ApiResponse(code = 400, message = FAILED_REORDER) })
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity update(@RequestBody AuthenticationItems authenticationItems,
                                 @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        try
        {
            providerManager.updateAuthenticationItems(authenticationItems);
            return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE, acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.BAD_REQUEST, FAILED_UPDATE, e, acceptHeader);
        }
    }
    
    @ApiOperation(value = "Reloads authenticators registry")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_RELOAD),
                            @ApiResponse(code = 500, message = FAILED_RELOAD) })
    @PutMapping(path = "/reload",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity reload(@RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        try
        {
            providerManager.reload();
            return getSuccessfulResponseEntity(SUCCESSFUL_RELOAD, acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_RELOAD, e, acceptHeader);
        }
    }
}
