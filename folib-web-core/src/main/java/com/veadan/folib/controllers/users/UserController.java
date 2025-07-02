package com.veadan.folib.controllers.users;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.users.support.TokenEntityBody;
import com.veadan.folib.controllers.users.support.UserOutput;
import com.veadan.folib.controllers.users.support.UserResponseEntity;
import com.veadan.folib.converts.UserConvert;
import com.veadan.folib.domain.PageResultResponse;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserPermissionForm;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.forms.users.UserForm;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.users.dto.UserPermissionDTO;
import com.veadan.folib.users.security.AuthoritiesProvider;
import com.veadan.folib.users.service.FolibRoleService;
import com.veadan.folib.users.service.RoleResourceRefService;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import com.veadan.folib.util.RSAUtils;
import com.veadan.folib.util.UserUtils;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.*;
import org.apache.commons.lang3.StringUtils;
import org.jose4j.lang.JoseException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
@Controller
@RequestMapping("/api/users")
@Api(description = "用户管理", tags = "用户管理")
public class UserController
        extends BaseController {

    public static final String SUCCESSFUL_CREATE_USER = "用户创建成功.";

    public static final String FAILED_CREATE_USER = "无法创建用户，因为提交的表单包含错误!";

    public static final String SUCCESSFUL_GET_USER = "已成功检索用户.";

    public static final String NOT_FOUND_USER = "指定的用户不存在!";

    public static final String SUCCESSFUL_GET_USERS = "已成功检索用户.";

    public static final String SUCCESSFUL_UPDATE_USER = "用户更新成功.";

    public static final String FAILED_UPDATE_USER = "由于提交的表单包含错误，无法更新用户!";

    public static final String SUCCESSFUL_DELETE_USER = "该用户已被删除.";

    public static final String FAILED_DELETE_USER = "无法删除用户.";

    public static final String OWN_USER_DELETE_FORBIDDEN = "无法删除自己";

    public static final String SUCCESSFUL_GENERATE_SECURITY_TOKEN = "安全令牌已生成.";

    public static final String FAILED_GENERATE_SECURITY_TOKEN = "无法生成 SecurityToken";

    public static final String USER_DELETE_FORBIDDEN = "禁止删除此帐户";

    public static final String PASSWORD_FIELD_IS_REQUIRED = "请传入密码字段(password)或者(originalPassword)";

    public static final String PASSWORD_PARSE_ERROR = "密码解析错误，请检查密码字段";

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private ConversionService conversionService;

    @Inject
    private AuthoritiesProvider authoritiesProvider;

    @Inject
    private PasswordEncoder passwordEncoder;

    @Inject
    private RSAUtils rsaUtils;
    @Inject
    private FolibRoleService folibRoleService;
    @Inject
    private StorageManagementService storageManagementService;
    @Autowired
    private RoleResourceRefService roleResourceRefService;

    @ApiOperation(value = "sync yaml users and roles")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GET_USERS)})
    //@PreAuthorize("hasAuthority('VIEW_USER_ROLE')")
    @GetMapping(produces = {MediaType.APPLICATION_JSON_VALUE}, path = "/syncYamlData")
    @ResponseBody
    public ResponseEntity syncYamlData() {
        //同步角色
        folibRoleService.syncYamlAuthorizationConfig();
        //同步存储空间用户
        storageManagementService.syncYamlStorageUsers(configurationManagementService.getConfiguration().getStorages().values());
        //同步用户
        boolean result = ((RelationalDatabaseUserService) userService).syncUser();

        return ResponseEntity.ok(result);
    }

    @ApiOperation(value = "user update ")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_UPDATE_USER),
            @ApiResponse(code = 400, message = FAILED_UPDATE_USER)})
    @PreAuthorize("hasAuthority('UPDATE_USER')")
    @PutMapping(value = "/storageUser", consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity updateStorageUser(@RequestBody @Validated UserPermissionForm userPermissionForm,
                                            BindingResult bindingResult,
                                            @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_CREATE_USER, bindingResult);
        }

        UserPermissionDTO userPermission = UserConvert.INSTANCE.UserPermissionFormToUserPermissionDTO(userPermissionForm);
        roleResourceRefService.updateStorageUser(userPermission);
        return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE_USER, accept);
    }

    @ApiOperation(value = "Used to retrieve all users")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GET_USERS)})
    @PreAuthorize("hasAuthority('VIEW_USER')")
    @GetMapping(produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity getUsers() {
        List<UserOutput> users = userService.getUsers()
                .getUsers()
                .stream()
                .sorted(Comparator.comparing(User::getUsername))
                .map(UserOutput::fromUser)
                .collect(Collectors.toList());

        return getJSONListResponseEntityBody("users", users);
    }

    @ApiOperation(value = "Used to retrieve users")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GET_USERS)})
    @PreAuthorize("hasAuthority('VIEW_USER')")
    @PostMapping(value = "/queryUser", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public TableResultResponse<UserOutput> queryUser(@RequestBody com.veadan.folib.users.dto.UserDto user, Integer page, Integer limit) {
        PageResultResponse<User> pageResultResponse = userService.queryUser(user, page, limit);
        if (Objects.isNull(pageResultResponse)) {
            return new TableResultResponse<>(0, Collections.emptyList());
        }
        List<User> userList = pageResultResponse.getData().getRows();
        List<UserOutput> userOutputList = Optional.ofNullable(userList).orElse(Collections.emptyList()).stream().map(UserOutput::fromUser).collect(Collectors.toList());
        return new TableResultResponse<>(pageResultResponse.getData().getTotal(), userOutputList);
    }

    @ApiOperation(value = "Used to retrieve a user")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GET_USER),
            @ApiResponse(code = 404, message = NOT_FOUND_USER)})
    @PreAuthorize("hasAuthority('VIEW_USER')")
    @GetMapping(value = "{username}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity getUser(@ApiParam(value = "The name of the user", required = true)
                                  @PathVariable String username,
                                  @RequestParam(value = "formFields",
                                          required = false,
                                          defaultValue = "false") Boolean includeFormFields,
                                  @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        User user = userService.findByUsername(username);
        if (user == null) {
            return getNotFoundResponseEntity(NOT_FOUND_USER, accept);
        }

        UserOutput userOutput = UserOutput.fromUser(user);
        UserResponseEntity responseEntity = new UserResponseEntity(userOutput);

        if (includeFormFields) {
            responseEntity.setAssignableRoles(authoritiesProvider.getAssignableRoles());
        }

        return ResponseEntity.ok(responseEntity);
    }

    @ApiOperation(value = "Used to create a new user")
    @AuditLog(value = AuditEventNameEnum.CREATE_USER, target = "#userForm.username")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_CREATE_USER),
            @ApiResponse(code = 400, message = FAILED_CREATE_USER)})
    @PreAuthorize("hasAuthority('CREATE_USER')")
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity create(@RequestBody @Validated(UserForm.NewUser.class) UserForm userForm,
                                 BindingResult bindingResult,
                                 @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_CREATE_USER, bindingResult);
        }
        if (StringUtils.isBlank(userForm.getPassword()) && StringUtils.isBlank(userForm.getOriginalPassword())) {
            return getBadRequestResponseEntity(PASSWORD_FIELD_IS_REQUIRED, accept);
        }
        com.veadan.folib.users.dto.UserDto user = conversionService.convert(userForm, com.veadan.folib.users.dto.UserDto.class);
        user.setUserGroupIds(userForm.getUserGroupIds());
        user.setNickname(userForm.getNickname());
        if (StringUtils.isNotBlank(user.getPassword())) {
            user.setOriginalPassword(user.getPassword());
            String password = rsaUtils.decrypt(user.getPassword());
            if (StringUtils.isBlank(password)) {
                return getBadRequestResponseEntity(PASSWORD_PARSE_ERROR, accept);
            }
            user.setPassword(password);
        } else if (StringUtils.isNotBlank(user.getOriginalPassword())) {
            String password = user.getOriginalPassword();
            user.setPassword(password);
            user.setOriginalPassword(rsaUtils.encrypt(password));
        }
        userService.save(new EncodedPasswordUser(user, passwordEncoder));
        return getSuccessfulResponseEntity(SUCCESSFUL_CREATE_USER, accept);
    }

    @ApiOperation(value = "Used to update an existing user")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_UPDATE_USER),
            @ApiResponse(code = 400, message = FAILED_UPDATE_USER),
            @ApiResponse(code = 403, message = USER_DELETE_FORBIDDEN)})
    @PreAuthorize("hasAuthority('UPDATE_USER')")
    @PutMapping(value = "{username}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity update(@ApiParam(value = "The name of the user", required = true)
                                 @PathVariable String username,
                                 @RequestBody @Validated(UserForm.ExistingUser.class) UserForm userToUpdate,
                                 BindingResult bindingResult,
                                 Authentication authentication,
                                 @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_UPDATE_USER, bindingResult);
        }

        if (!(authentication.getPrincipal() instanceof UserDetails)) {
            String message = "Unsupported logged user principal type: " + authentication.getPrincipal().getClass();
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, accept);
        }

        final User existsUser = userService.findByUsername(username);
        if (Objects.isNull(existsUser))
        {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, NOT_FOUND_USER, accept);
        }

        com.veadan.folib.users.dto.UserDto user = conversionService.convert(userToUpdate, com.veadan.folib.users.dto.UserDto.class);
        user.setUserGroupIds(userToUpdate.getUserGroupIds());
        user.setNickname(userToUpdate.getNickname());
        if (StringUtils.isNotBlank(user.getPassword())) {
            user.setOriginalPassword(user.getPassword());
            String password = rsaUtils.decrypt(user.getPassword());
            if (StringUtils.isBlank(password)) {
                return getBadRequestResponseEntity(PASSWORD_PARSE_ERROR, accept);
            }
            user.setPassword(password);
        } else if (StringUtils.isNotBlank(user.getOriginalPassword())) {
            String password = user.getOriginalPassword();
            user.setPassword(password);
            user.setOriginalPassword(rsaUtils.encrypt(password));
        }
        userService.save(new EncodedPasswordUser(user, passwordEncoder));
        return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE_USER, accept);
    }

    @ApiOperation(value = "Deletes a user from a repository.")
    @AuditLog(value = AuditEventNameEnum.DELETE_USER, target = "#username")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_DELETE_USER),
            @ApiResponse(code = 400, message = FAILED_DELETE_USER),
            @ApiResponse(code = 403, message = USER_DELETE_FORBIDDEN),
            @ApiResponse(code = 404, message = NOT_FOUND_USER)})
    @PreAuthorize("hasAuthority('DELETE_USER')")
    @DeleteMapping(value = "{username}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity delete(@ApiParam(value = "The name of the user") @PathVariable String username,
                                 Authentication authentication,
                                 @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (!(authentication.getPrincipal() instanceof UserDetails)) {
            String message = "Unsupported logged user principal type: " + authentication.getPrincipal().getClass();
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, accept);
        }

        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        if (StringUtils.equals(loggedUser.getUsername(), username)) {
            return getFailedResponseEntity(HttpStatus.FORBIDDEN, OWN_USER_DELETE_FORBIDDEN, accept);
        }

        if (StringUtils.equals("admin", username)) {
            return getFailedResponseEntity(HttpStatus.FORBIDDEN, USER_DELETE_FORBIDDEN, accept);
        }

        User user = userService.findByUsername(username);
        if (user == null) {
            return getNotFoundResponseEntity(NOT_FOUND_USER, accept);
        }

        userService.deleteByUsername(user.getUsername());

        return getSuccessfulResponseEntity(SUCCESSFUL_DELETE_USER, accept);
    }

    @ApiOperation(value = "Generate new security token for specified user.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GENERATE_SECURITY_TOKEN),
            @ApiResponse(code = 400, message = FAILED_GENERATE_SECURITY_TOKEN),
            @ApiResponse(code = 404, message = NOT_FOUND_USER)})
    @PreAuthorize("hasAuthority('UPDATE_USER')")
    @GetMapping(value = "{username}/generate-security-token",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity generateSecurityToken(@ApiParam(value = "The name of the user") @PathVariable String username,
                                                @ApiParam(value = "Token effective seconds") @RequestParam(required = false) Integer expireSeconds,
                                                @RequestHeader(HttpHeaders.ACCEPT) String accept)
            throws JoseException {
        User user = userService.findByUsername(username);
        if (user == null) {
            return getNotFoundResponseEntity(NOT_FOUND_USER, accept);
        }
        String securityToken = userService.generateSecurityToken(username, expireSeconds);
        if (securityToken == null) {
            String message = String.format("Failed to generate SecurityToken, probably you should first set " +
                    "SecurityTokenKey for the user: %s", username);

            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, accept);
        }

        Object body = getTokenEntityBody(securityToken, accept);

        return ResponseEntity.ok(body);
    }

    @ApiOperation(value = "Generate a new security token for the current user.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_GENERATE_SECURITY_TOKEN),
            @ApiResponse(code = 400, message = FAILED_GENERATE_SECURITY_TOKEN),
            @ApiResponse(code = 404, message = NOT_FOUND_USER)})
    @GetMapping(value = "/generate-current-security-token",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity generateCurrentSecurityToken(@ApiParam(value = "Token effective seconds") @RequestParam(required = false) Integer expireSeconds,
                                                       @RequestHeader(HttpHeaders.ACCEPT) String accept)
            throws JoseException {
        String username = UserUtils.getUsername();
        if (StringUtils.isBlank(username)) {
            return getNotFoundResponseEntity(NOT_FOUND_USER, accept);
        }
        User user = userService.findByUsername(username);
        if (user == null) {
            return getNotFoundResponseEntity(NOT_FOUND_USER, accept);
        }
        String securityToken = userService.generateSecurityToken(username, expireSeconds);
        if (securityToken == null) {
            String message = String.format("Failed to generate SecurityToken, probably you should first set " +
                    "SecurityTokenKey for the user: %s", username);

            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, accept);
        }

        Object body = getTokenEntityBody(securityToken, accept);

        return ResponseEntity.ok(body);
    }

    @ApiOperation(value = "Obtain the encrypted original password.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = NOT_FOUND_USER)})
    @GetMapping(value = "{username}/encoded",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity get(@ApiParam(value = "The name of the user") @PathVariable String username,
                              @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        User user = userService.findByUsername(username);
        if (user == null) {
            return getNotFoundResponseEntity(NOT_FOUND_USER, accept);
        }
        return ResponseEntity.ok(user.getOriginalPassword());
    }

    private Object getTokenEntityBody(String token,
                                      String accept) {
        if (MediaType.APPLICATION_JSON_VALUE.equals(accept)) {
            return new TokenEntityBody(token);
        } else {
            return token;
        }
    }
}
