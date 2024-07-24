package com.veadan.folib.controllers;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.controllers.users.UserController;
import com.veadan.folib.controllers.users.support.UserOutput;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserRepositoryPermission;
import com.veadan.folib.dto.UserDTO;
import com.veadan.folib.dto.UserRoleDTO;
import com.veadan.folib.forms.users.UserForm;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.service.RoleResourceRefService;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.util.RSAUtils;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.PageRequest;
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
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Fengmaogen
 */
@RestController
@RequestMapping("/api/auth")
@Api(value = "权限管理",tags = "权限管理")
public class RoleController extends BaseController {

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private PasswordEncoder passwordEncoder;
    @Inject
    private ConfigurationManager configurationManager;
    @Inject
    private RSAUtils rsaUtils;
    @Inject
    private RoleResourceRefService roleResourceRefService;
    
    @ApiOperation(value = "获取用户的关联角色")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Returns account details"),
                            @ApiResponse(code = 403, message = "Unauthenticated access or user account has been disabled"),
                            @ApiResponse(code = 404, message = UserController.NOT_FOUND_USER) })
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @GetMapping(value = "/{userName}",
                produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity getAccount(@PathVariable String userName)
    {
        PageRequest pageRequest = PageRequest.of(0, 100);
        List<UserRoleDTO> rolesByUserName = roleResourceRefService.getRolesByUserName(userName, pageRequest);

        return ResponseEntity.ok(rolesByUserName);
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
        if (StringUtils.isNotBlank(userToUpdate.getPassword())) {
            user.setOriginalPassword(userToUpdate.getPassword());
            String password = rsaUtils.decrypt(userToUpdate.getPassword());
            user.setPassword(password);
        }
        user.setEmail(userToUpdate.getEmail());
        user.setSecurityTokenKey(userToUpdate.getSecurityTokenKey());
        user.setAvatar(userToUpdate.getAvatar());
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
    public ResponseEntity<UserRepositoryPermission> getStorageAndRepositoryPermission(Authentication authentication, @ApiParam(value = "The storageId", required = true) @PathVariable String storageId, @ApiParam(value = "The repositoryId", required = true) @PathVariable String repositoryId) {
        SpringSecurityUser userDetails = (SpringSecurityUser)authentication.getPrincipal();
        Storage storage = configurationManager.getStorage(storageId);
        Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(storageId, repositoryId, null);
        UserRepositoryPermission userRepositoryPermission = UserRepositoryPermission.builder().storageAdmin(storage.getAdmin()).permissions(storageAuthorities.stream().map(Privileges::getAuthority).collect(Collectors.toSet())).build();
        return ResponseEntity.ok(userRepositoryPermission);
    }
}
