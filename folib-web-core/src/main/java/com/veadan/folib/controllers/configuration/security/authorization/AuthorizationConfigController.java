package com.veadan.folib.controllers.configuration.security.authorization;

import java.io.IOException;
import java.util.List;
import java.util.function.Supplier;

import javax.inject.Inject;
import javax.xml.bind.JAXBException;

import com.veadan.folib.forms.PrivilegeListForm;
import com.veadan.folib.forms.RoleForm;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.DatabaseUserService.Database;
import com.veadan.folib.validation.RequestBodyValidationException;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.web.authentication.AnonymousAuthenticationFilter;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * @author Pablo Tirado
 */
@Controller
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping(value = "/api/configuration/authorization")
@Api(value = "/api/configuration/authorization")
public class AuthorizationConfigController
        extends BaseController
{

    static final String SUCCESSFUL_ADD_ROLE = "The role was created successfully.";
    static final String FAILED_ADD_ROLE = "Role cannot be saved because the submitted form contains errors!";

    static final String SUCCESSFUL_GET_CONFIG = "Everything went ok.";
    static final String FAILED_GET_CONFIG = "Could not retrieve the folib-authorization.yaml configuration file.";

    static final String SUCCESSFUL_DELETE_ROLE = "The role was deleted.";
    static final String FAILED_DELETE_ROLE = "Could not delete the role.";

    static final String SUCCESSFUL_ASSIGN_PRIVILEGES = "The privileges were assigned.";
    static final String FAILED_ASSIGN_PRIVILEGES = "Privileges cannot be saved because the submitted form contains errors!";

    static final String AUTHORIZATION_CONFIG_OPERATION_FAILED = "Error during config processing.";

    @Inject
    private AuthorizationConfigService authorizationConfigService;

    @Inject
    @Database
    private UserService userService;

    @Inject
    private AnonymousAuthenticationFilter anonymousAuthenticationFilter;

    @Inject
    private ConversionService conversionService;


    @ApiOperation(value = "Used to add new roles")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_ADD_ROLE),
                            @ApiResponse(code = 400, message = FAILED_ADD_ROLE) })
    @PostMapping(value = "/role",
                 consumes = MediaType.APPLICATION_JSON_VALUE,
                 produces = { MediaType.TEXT_PLAIN_VALUE, MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity addRole(@RequestBody @Validated RoleForm roleForm,
                                  BindingResult bindingResult,
                                  @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_ADD_ROLE, bindingResult);
        }

        RoleDto role = conversionService.convert(roleForm, RoleDto.class);

        authorizationConfigService.addRole(role);

        return processConfig(() -> SUCCESSFUL_ADD_ROLE, acceptHeader);
    }

    @ApiOperation(value = "Retrieves the folib-authorization.yaml configuration file.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_GET_CONFIG),
                            @ApiResponse(code = 500, message = FAILED_GET_CONFIG) })
    @GetMapping(produces = { com.veadan.folib.net.MediaType.APPLICATION_YAML_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getAuthorizationConfig(@RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        return processConfig(null, acceptHeader);
    }

    @ApiOperation(value = "Deletes a role by name.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_DELETE_ROLE),
                            @ApiResponse(code = 400, message = FAILED_DELETE_ROLE) })
    @DeleteMapping(value = "/role/{name}",
                   produces = { MediaType.TEXT_PLAIN_VALUE, MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity deleteRole(@ApiParam(value = "The name of the role", required = true)
                                     @PathVariable("name") String name,
                                     @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        try
        {
            deleteRole(name);
        }
        catch (RuntimeException e)
        {
            String message = e.getMessage();
            logger.error(message, e);
            return getBadRequestResponseEntity(message, acceptHeader);
        }

        return processConfig(() -> SUCCESSFUL_DELETE_ROLE, acceptHeader);
    }

    private void deleteRole(String name) throws IOException
    {
        if (authorizationConfigService.deleteRole(name))
        {
            // revoke role from every user that exists in the system
            userService.revokeEveryone(name.toUpperCase());
        }
        else
        {
            throw new RuntimeException(FAILED_DELETE_ROLE);
        }
    }

    @ApiOperation(value = "Used to assign privileges to the anonymous user")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_ASSIGN_PRIVILEGES),
                            @ApiResponse(code = 400, message = FAILED_ASSIGN_PRIVILEGES) })
    @PostMapping(value = "/anonymous/privileges",
                 consumes = MediaType.APPLICATION_JSON_VALUE,
                 produces = { MediaType.TEXT_PLAIN_VALUE, MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity addPrivilegesToAnonymous(@RequestBody @Validated PrivilegeListForm privilegeListForm,
                                                   BindingResult bindingResult,
                                                   @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_ASSIGN_PRIVILEGES, bindingResult);
        }

        List<Privileges> privilegeList = privilegeListForm.getPrivileges();
        authorizationConfigService.addPrivilegesToAnonymous(privilegeList);
        addAnonymousAuthority(privilegeList);

        return processConfig(() -> SUCCESSFUL_ASSIGN_PRIVILEGES, acceptHeader);
    }

    private ResponseEntity processConfig(Supplier<String> successMessage,
                                         String acceptHeader)
    {
        return processConfig(successMessage, ResponseEntity::ok, acceptHeader);
    }

    private ResponseEntity processConfig(Supplier<String> successMessage,
                                         CustomSuccessResponseBuilder customSuccessResponseBuilder,
                                         String acceptHeader)
    {
        try
        {
            if (successMessage != null)
            {
                return getSuccessfulResponseEntity(successMessage.get(), acceptHeader);

            }
            else
            {
                return customSuccessResponseBuilder.build(authorizationConfigService.getDto());
            }
        }
        catch (RuntimeException e)
        {
            String message = e.getMessage();
            return getExceptionResponseEntity(HttpStatus.BAD_REQUEST, message, e, acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR,
                                              AUTHORIZATION_CONFIG_OPERATION_FAILED,
                                              e,
                                              acceptHeader);
        }
    }

    private void addAnonymousAuthority(List<Privileges> authorities)
    {
        authorities.stream().forEach(this::addAnonymousAuthority);
    }

    private void addAnonymousAuthority(Privileges authority)
    {
        anonymousAuthenticationFilter.getAuthorities().add(authority);
    }

    private interface CustomSuccessResponseBuilder
    {
        ResponseEntity build(AuthorizationConfigDto config)throws JAXBException;
    }

}
