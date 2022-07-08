package com.veadan.folib.controllers.configuration.security.ldap;

import javax.inject.Inject;

import com.veadan.folib.authentication.api.ldap.LdapAuthenticationConfigurationManager;
import com.veadan.folib.authentication.api.ldap.LdapConfiguration;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.forms.configuration.security.ldap.LdapConfigurationTestForm;
import com.veadan.folib.validation.RequestBodyValidationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.AuthenticationException;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
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
 * @author veadan
 * @author Pablo Tirado
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping(value = "/api/configuration/ldap")
@Api(value = "/api/configuration/ldap")
public class LdapAuthenticatorConfigurationController
        extends BaseController
{

    private static final String FAILED_PUT_LDAP = "LDAP configuration cannot be updated because the submitted form contains errors!";

    private static final String FAILED_PUT_LDAP_TEST = "LDAP configuration cannot be tested because the submitted form contains errors!";

    private static final String ERROR_PUT_LDAP = "Failed to update LDAP configuration.";

    private static final String SUCCESS_PUT_LDAP = "LDAP configuration update succeeded";

    private static final String LDAP_TEST_PASSED = "LDAP configuration test passed";

    private static final String LDAP_TEST_FAILED = "LDAP configuration test failed";

    private static final String ERROR_PUT_LDAP_TEST = "Failed to test LDAP configuration.";

    @Inject
    private LdapAuthenticationConfigurationManager ldapAuthenticationManager;
    
    @ApiOperation(value = "Tests LDAP configuration settings")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "LDAP configuration test has passed.") })
    @PutMapping(value = "/test", produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity testLdapConfiguration(@RequestBody @Validated LdapConfigurationTestForm form,
                                                BindingResult bindingResult,
                                                @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_PUT_LDAP_TEST, bindingResult);
        }

        try
        {
            ldapAuthenticationManager.testConfiguration(form.getUsername(), 
                                                        form.getPassword(),
                                                        form.getConfiguration());
        }
        catch (AuthenticationException e)
        {
            return getSuccessfulResponseEntity(LDAP_TEST_FAILED, acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, ERROR_PUT_LDAP_TEST, e, acceptHeader);
        }

        return getSuccessfulResponseEntity(LDAP_TEST_PASSED, acceptHeader);
    }

    @ApiOperation(value = "Update the LDAP configuration settings")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "LDAP configuration updated successfully.") })
    @PutMapping(produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity putLdapConfiguration(@RequestBody @Validated LdapConfiguration configuration,
                                               BindingResult bindingResult,
                                               @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_PUT_LDAP, bindingResult);
        }

        try
        {
            ldapAuthenticationManager.updateConfiguration(configuration);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, ERROR_PUT_LDAP, e, acceptHeader);
        }

        return getSuccessfulResponseEntity(SUCCESS_PUT_LDAP, acceptHeader);
    }

    @ApiOperation(value = "Returns LDAP configuration")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The LDAP configuration.") })
    @GetMapping(produces = { MediaType.APPLICATION_JSON_VALUE })
    public LdapConfiguration getLdapConfiguration(@RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        return ldapAuthenticationManager.getConfiguration();
    }

}
