package com.veadan.folib.controllers.configuration.security.authorization;

import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.dto.PrivilegeListDto;
import com.veadan.folib.dto.RoleDto;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.*;
import jakarta.xml.bind.JAXBException;
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
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.io.IOException;
import java.util.List;
import java.util.function.Supplier;

/**
 * @author Veadan
 */
@Controller
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping(value = "/api/configuration/authorization")
@Api(description = "用户角色管理",tags = "用户角色管理")
public class AuthorizationConfigController
        extends BaseController
{

    static final String SUCCESSFUL_ADD_ROLE = "角色创建成功";
    static final String FAILED_ADD_ROLE = "由于提交的表单包含错误，无法保存角色";

    static final String SUCCESSFUL_GET_CONFIG = "一切顺利";
    static final String FAILED_GET_CONFIG = "无法检索 folib-authorization.yaml 配置文件";

    static final String SUCCESSFUL_DELETE_ROLE = "该角色已被删除";
    static final String FAILED_DELETE_ROLE = "无法删除角色";

    static final String SUCCESSFUL_ASSIGN_PRIVILEGES = "特权已分配";
    static final String FAILED_ASSIGN_PRIVILEGES = "由于提交的表单包含错误，无法保存权限！";

    static final String AUTHORIZATION_CONFIG_OPERATION_FAILED = "配置处理期间出错";

    @Inject
    private AuthorizationConfigService authorizationConfigService;

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private AnonymousAuthenticationFilter anonymousAuthenticationFilter;

    @Inject
    private ConversionService conversionService;


    @ApiOperation(value = "用于添加新角色")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_ADD_ROLE),
                            @ApiResponse(code = 400, message = FAILED_ADD_ROLE) })
    @PostMapping(value = "/role",
                 consumes = MediaType.APPLICATION_JSON_VALUE,
                 produces = { MediaType.TEXT_PLAIN_VALUE, MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity addRole(@RequestBody @Validated RoleDto roleForm,
                                  BindingResult bindingResult,
                                  @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_ADD_ROLE, bindingResult);
        }

        com.veadan.folib.authorization.dto.RoleDto role = conversionService.convert(roleForm, com.veadan.folib.authorization.dto.RoleDto.class);

        authorizationConfigService.addRole(role);

        return processConfig(() -> SUCCESSFUL_ADD_ROLE, acceptHeader);
    }

    @ApiOperation(value = "检索 folib-authorization.yaml 配置文件")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_GET_CONFIG),
                            @ApiResponse(code = 500, message = FAILED_GET_CONFIG) })
    @GetMapping(produces = { com.veadan.folib.net.MediaType.APPLICATION_YAML_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getAuthorizationConfig(@RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        return processConfig(null, acceptHeader);
    }

    @ApiOperation(value = "按名称删除角色")
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

    @ApiOperation(value = "用于为匿名用户分配权限")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_ASSIGN_PRIVILEGES),
                            @ApiResponse(code = 400, message = FAILED_ASSIGN_PRIVILEGES) })
    @PostMapping(value = "/anonymous/privileges",
                 consumes = MediaType.APPLICATION_JSON_VALUE,
                 produces = { MediaType.TEXT_PLAIN_VALUE, MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity addPrivilegesToAnonymous(@RequestBody @Validated PrivilegeListDto privilegeListForm,
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
