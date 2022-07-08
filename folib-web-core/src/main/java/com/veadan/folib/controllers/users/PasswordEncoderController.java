package com.veadan.folib.controllers.users;

import com.veadan.folib.forms.users.PasswordEncodeForm;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.validation.RequestBodyValidationException;

import javax.inject.Inject;
import java.util.Collections;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * @author veadan
 */
@Controller
@RequestMapping("/api/users/encrypt/password")
@Api(value = "/api/users/encrypt/encoder")
@PreAuthorize("hasAuthority('ADMIN')")
public class PasswordEncoderController
        extends BaseController
{

    public static final String INVALID_FORM = "Form contains invalid data!";

    @Inject
    private PasswordEncoder passwordEncoder;

    @ApiOperation(value = "Encodes submitted raw password")
    @ApiResponses(value = @ApiResponse(code = 200, message = "Returns encoded password"))
    @PostMapping(consumes = { MediaType.APPLICATION_JSON_VALUE },
                 produces = { MediaType.APPLICATION_JSON_VALUE,
                              MediaType.TEXT_PLAIN_VALUE })
    @ResponseBody
    public ResponseEntity encode(@RequestHeader(HttpHeaders.ACCEPT) String accept,
                                 @Validated @RequestBody PasswordEncodeForm form,
                                 BindingResult bindingResult)
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(INVALID_FORM, bindingResult);
        }

        String encoded = passwordEncoder.encode(form.getPassword());
        Object response = encoded;

        if (accept.equals(MediaType.APPLICATION_JSON_VALUE))
        {
            response = Collections.singletonMap("password", encoded);
        }

        return ResponseEntity.ok().body(response);
    }

}
