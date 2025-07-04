package com.veadan.folib.controllers.configuration.security.ldap;

import javax.inject.Inject;
import com.veadan.folib.authentication.api.ldap.LdapAuthenticationConfigurationManager;
import com.veadan.folib.authentication.api.ldap.LdapConfiguration;
import com.veadan.folib.components.common.CommonComponent;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.forms.configuration.security.ldap.LdapConfigurationTestForm;
import com.veadan.folib.validation.RequestBodyValidationException;
import org.springframework.context.annotation.Lazy;
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
 * @author Veadan
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping(value = "/api/configuration/ldap")
@Api(description = "ldap配置管理",tags = "ldap配置管理")
public class LdapAuthenticatorConfigurationController
        extends BaseController
{

    private static final String FAILED_PUT_LDAP = "无法更新 LDAP 配置，因为提交的表单包含错误！";

    private static final String FAILED_PUT_LDAP_TEST = "无法测试 LDAP 配置，因为提交的表单包含错误!";

    private static final String ERROR_PUT_LDAP = "更新 LDAP 配置失败.";

    private static final String SUCCESS_PUT_LDAP = "LDAP 配置更新成功";

    private static final String LDAP_TEST_PASSED = "LDAP 配置测试通过";

    private static final String LDAP_TEST_FAILED = "LDAP 配置测试失败";

    private static final String ERROR_PUT_LDAP_TEST = "未能测试 LDAP 配置。";

    @Inject
    private LdapAuthenticationConfigurationManager ldapAuthenticationManager;

    @Inject
    @Lazy
    private CommonComponent commonComponent;
    
    @ApiOperation(value = "测试 LDAP 配置设置")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "LDAP 配置测试已通过。") })
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


    @ApiOperation(value = "更新 LDAP 配置设置")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "LDAP 配置更新成功。") })
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
            commonComponent.updateLdap(configuration);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, ERROR_PUT_LDAP, e, acceptHeader);
        }

        return getSuccessfulResponseEntity(SUCCESS_PUT_LDAP, acceptHeader);
    }

    @ApiOperation(value = "返回 LDAP 配置")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "LDAP 配置。") })
    @GetMapping(produces = { MediaType.APPLICATION_JSON_VALUE })
    public LdapConfiguration getLdapConfiguration(@RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        return ldapAuthenticationManager.getConfiguration();
    }

}
