package com.veadan.folib.controllers;

import javax.inject.Inject;

import com.veadan.folib.controllers.users.UserController;
import com.veadan.folib.controllers.users.support.UserOutput;
import com.veadan.folib.forms.users.UserForm;
import com.veadan.folib.domain.User;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import com.veadan.folib.users.service.impl.DatabaseUserService.Database;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.*;
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

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Steve Todorov
 */
@Controller
@RequestMapping("/api/account")
@Api(value = "/api/account")
public class AccountController
        extends BaseController
{

    @Inject
    @Database
    private UserService userService;

    @Inject
    private PasswordEncoder passwordEncoder;
    
    @ApiOperation(value = "获取当前登录用户的帐户详细信息")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Returns account details"),
                            @ApiResponse(code = 403, message = "Unauthenticated access or user account has been disabled"),
                            @ApiResponse(code = 404, message = UserController.NOT_FOUND_USER) })
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @GetMapping(value = "",
                produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity getAccount(Authentication authentication)
    {

        if (!(authentication.getPrincipal() instanceof UserDetails))
        {
            String message = "Unsupported logged user principal type: " + authentication.getPrincipal().getClass();
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, MediaType.APPLICATION_JSON_VALUE);
        }

        User user = userService.findByUsername(authentication.getName());
        if (user == null)
        {
            return getNotFoundResponseEntity(UserController.NOT_FOUND_USER, MediaType.APPLICATION_JSON_VALUE);
        }

        return ResponseEntity.ok(UserOutput.fromUser(user));
    }

    @ApiOperation(value = "Get the account details of the currently logged user")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Account details have been successfully updated"),
                            @ApiResponse(code = 400, message = "Unsupported logged user principal type"),
                            @ApiResponse(code = 404, message = UserController.NOT_FOUND_USER) })
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @PutMapping(value = "",
                consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity updateAccount(@RequestBody @Validated(UserForm.UpdateAccount.class) UserForm userToUpdate,
                                        BindingResult bindingResult,
                                        Authentication authentication)
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(UserController.FAILED_UPDATE_USER, bindingResult);
        }

        if (!(authentication.getPrincipal() instanceof UserDetails))
        {
            String message = "Unsupported logged user principal type: " + authentication.getPrincipal().getClass();
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, MediaType.APPLICATION_JSON_VALUE);
        }

        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();

        // Updating account details currently only allows changing password and security token.
        // However, we're reusing the UserForm which includes other fields. Just to be on the safe side,
        // we are creating a new UserDto which contains only password & securityToken field changes.
        UserDto user = new UserDto();
        user.setUsername(loggedUser.getUsername());
        user.setPassword(userToUpdate.getPassword());
        user.setEmail(userToUpdate.getEmail());
        user.setSecurityTokenKey(userToUpdate.getSecurityTokenKey());

        userService.updateAccountDetailsByUsername(new EncodedPasswordUser(user, passwordEncoder));

        return getSuccessfulResponseEntity("Account details have been successfully updated",
                                           MediaType.APPLICATION_JSON_VALUE);
    }

    @ApiOperation(value = "获取当前用户对指定存储空间和仓库的权限信息")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Returns permissions details")})
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @GetMapping(value = "/permission/{storageId}/{repositoryId}",
            produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity<Set<String>> getStorageAndRepositoryPermission(Authentication authentication, @ApiParam(value = "The storageId", required = true) @PathVariable String storageId, @ApiParam(value = "The repositoryId", required = true) @PathVariable String repositoryId) {
        SpringSecurityUser userDetails = (SpringSecurityUser)authentication.getPrincipal();
        Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(storageId, repositoryId);
        return ResponseEntity.ok(storageAuthorities.stream().map(Privileges::getAuthority).collect(Collectors.toSet()));
    }
}
