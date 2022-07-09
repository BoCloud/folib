package com.veadan.folib.controllers.users;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import com.veadan.folib.controllers.users.support.TokenEntityBody;
import com.veadan.folib.controllers.users.support.UserOutput;
import com.veadan.folib.controllers.users.support.UserResponseEntity;
import com.veadan.folib.forms.users.UserForm;
import org.apache.commons.lang3.StringUtils;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.domain.User;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.security.AuthoritiesProvider;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import com.veadan.folib.users.service.impl.DatabaseUserService.Database;
import com.veadan.folib.validation.RequestBodyValidationException;
import org.jose4j.lang.JoseException;
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
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * @author Veadan
 */
@Controller
@RequestMapping("/api/users")
@Api(value = "/api/users")
public class UserController
        extends BaseController
{

    public static final String SUCCESSFUL_CREATE_USER = "The user was created successfully.";

    public static final String FAILED_CREATE_USER = "User cannot be created because the submitted form contains errors!";

    public static final String SUCCESSFUL_GET_USER = "User was retrieved successfully.";

    public static final String NOT_FOUND_USER = "The specified user does not exist!";

    public static final String SUCCESSFUL_GET_USERS = "Users were retrieved successfully.";

    public static final String SUCCESSFUL_UPDATE_USER = "The user was updated successfully.";

    public static final String FAILED_UPDATE_USER = "User cannot be updated because the submitted form contains errors!";

    public static final String SUCCESSFUL_DELETE_USER = "The user was deleted.";

    public static final String FAILED_DELETE_USER = "Could not delete the user.";

    public static final String OWN_USER_DELETE_FORBIDDEN = "Unable to delete yourself";

    public static final String SUCCESSFUL_GENERATE_SECURITY_TOKEN = "The security token was generated.";

    public static final String FAILED_GENERATE_SECURITY_TOKEN = "Failed to generate SecurityToken";

    public static final String SUCCESSFUL_UPDATE_ACCESS_MODEL = "The custom access model was updated.";

    public static final String FAILED_UPDATE_ACCESS_MODEL = "Could not update the access model.";

    public static final String USER_DELETE_FORBIDDEN = "Deleting this account is forbidden!";

    @Inject
    @Database
    private UserService userService;

    @Inject
    private ConversionService conversionService;

    @Inject
    private AuthoritiesProvider authoritiesProvider;

    @Inject
    private PasswordEncoder passwordEncoder;
    
    @ApiOperation(value = "Used to retrieve all users")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_GET_USERS) })
    @PreAuthorize("hasAuthority('VIEW_USER')")
    @GetMapping(produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity getUsers()
    {
        List<UserOutput> users = userService.getUsers()
                                            .getUsers()
                                            .stream()
                                            .sorted(Comparator.comparing(User::getUsername))
                                            .map(UserOutput::fromUser)
                                            .collect(Collectors.toList());

        return getJSONListResponseEntityBody("users", users);
    }

    @ApiOperation(value = "Used to retrieve a user")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_GET_USER),
                            @ApiResponse(code = 404, message = NOT_FOUND_USER) })
    @PreAuthorize("hasAuthority('VIEW_USER')")
    @GetMapping(value = "{username}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity getUser(@ApiParam(value = "The name of the user", required = true)
                                  @PathVariable String username,
                                  @RequestParam(value = "formFields",
                                                required = false,
                                                defaultValue = "false") Boolean includeFormFields,
                                  @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        User user = userService.findByUsername(username);
        if (user == null)
        {
            return getNotFoundResponseEntity(NOT_FOUND_USER, accept);
        }

        UserOutput userOutput = UserOutput.fromUser(user);
        UserResponseEntity responseEntity = new UserResponseEntity(userOutput);

        if (includeFormFields)
        {
            responseEntity.setAssignableRoles(authoritiesProvider.getAssignableRoles());
        }

        return ResponseEntity.ok(responseEntity);
    }

    @ApiOperation(value = "Used to create a new user")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_CREATE_USER),
                            @ApiResponse(code = 400, message = FAILED_CREATE_USER) })
    @PreAuthorize("hasAuthority('CREATE_USER')")
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity create(@RequestBody @Validated(UserForm.NewUser.class) UserForm userForm,
                                 BindingResult bindingResult,
                                 @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_CREATE_USER, bindingResult);
        }

        UserDto user = conversionService.convert(userForm, UserDto.class);
        userService.save(new EncodedPasswordUser(user, passwordEncoder));

        return getSuccessfulResponseEntity(SUCCESSFUL_CREATE_USER, accept);
    }

    @ApiOperation(value = "Used to update an existing user")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_UPDATE_USER),
                            @ApiResponse(code = 400, message = FAILED_UPDATE_USER),
                            @ApiResponse(code = 403, message = USER_DELETE_FORBIDDEN) })
    @PreAuthorize("hasAuthority('UPDATE_USER')")
    @PutMapping(value = "{username}",
                consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity update(@ApiParam(value = "The name of the user", required = true)
                                 @PathVariable String username,
                                 @RequestBody @Validated(UserForm.ExistingUser.class) UserForm userToUpdate,
                                 BindingResult bindingResult,
                                 Authentication authentication,
                                 @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_UPDATE_USER, bindingResult);
        }

        if (!(authentication.getPrincipal() instanceof UserDetails))
        {
            String message = "Unsupported logged user principal type: " + authentication.getPrincipal().getClass();
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, accept);
        }

        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        if (StringUtils.equals(loggedUser.getUsername(), username))
        {
            return getFailedResponseEntity(HttpStatus.FORBIDDEN, OWN_USER_DELETE_FORBIDDEN, accept);
        }

        UserDto user = conversionService.convert(userToUpdate, UserDto.class);
        userService.save(new EncodedPasswordUser(user, passwordEncoder));

        return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE_USER, accept);
    }

    @ApiOperation(value = "Deletes a user from a repository.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_DELETE_USER),
                            @ApiResponse(code = 400, message = FAILED_DELETE_USER),
                            @ApiResponse(code = 403, message = USER_DELETE_FORBIDDEN),
                            @ApiResponse(code = 404, message = NOT_FOUND_USER) })
    @PreAuthorize("hasAuthority('DELETE_USER')")
    @DeleteMapping(value = "{username}",
                   produces = { MediaType.TEXT_PLAIN_VALUE,
                                MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity delete(@ApiParam(value = "The name of the user") @PathVariable String username,
                                 Authentication authentication,
                                 @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        if (!(authentication.getPrincipal() instanceof UserDetails))
        {
            String message = "Unsupported logged user principal type: " + authentication.getPrincipal().getClass();
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, accept);
        }

        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        if (StringUtils.equals(loggedUser.getUsername(), username))
        {
            return getFailedResponseEntity(HttpStatus.FORBIDDEN, OWN_USER_DELETE_FORBIDDEN, accept);
        }

        if (StringUtils.equals("admin", username))
        {
            return getFailedResponseEntity(HttpStatus.FORBIDDEN, USER_DELETE_FORBIDDEN, accept);
        }

        User user = userService.findByUsername(username);
        if (user == null)
        {
            return getNotFoundResponseEntity(NOT_FOUND_USER, accept);
        }

        userService.deleteByUsername(user.getUsername());

        return getSuccessfulResponseEntity(SUCCESSFUL_DELETE_USER, accept);
    }

    @ApiOperation(value = "Generate new security token for specified user.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_GENERATE_SECURITY_TOKEN),
                            @ApiResponse(code = 400, message = FAILED_GENERATE_SECURITY_TOKEN),
                            @ApiResponse(code = 404, message = NOT_FOUND_USER) })
    @PreAuthorize("hasAuthority('UPDATE_USER')")
    @GetMapping(value = "{username}/generate-security-token",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity generateSecurityToken(@ApiParam(value = "The name of the user") @PathVariable String username,
                                                @RequestHeader(HttpHeaders.ACCEPT) String accept)
            throws JoseException
    {
        User user = userService.findByUsername(username);
        if (user == null)
        {
            return getNotFoundResponseEntity(NOT_FOUND_USER, accept);
        }

        String securityToken = userService.generateSecurityToken(username);
        if (securityToken == null)
        {
            String message = String.format("Failed to generate SecurityToken, probably you should first set " +
                                           "SecurityTokenKey for the user: %s", username);

            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, accept);
        }

        Object body = getTokenEntityBody(securityToken, accept);

        return ResponseEntity.ok(body);
    }

    private Object getTokenEntityBody(String token,
                                      String accept)
    {
        if (MediaType.APPLICATION_JSON_VALUE.equals(accept))
        {
            return new TokenEntityBody(token);
        }
        else
        {
            return token;
        }
    }

}
