package com.veadan.folib.controllers;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * @author Steve Todorov
 */
@Controller
@RequestMapping("/api/ping")
@Api(value = "/api/ping")
public class PingController
        extends BaseController
{

    static final String READY_STREAM_VALUE = "event:ready\ndata: \n\n";

    @ApiResponses(value = { @ApiResponse(code = 200, message = "Strongbox is up and working.") })
    @GetMapping(produces = { MediaType.TEXT_EVENT_STREAM_VALUE + ";charset=UTF-8" })
    public ResponseEntity ping()
    {
        return ResponseEntity.ok().header(HttpHeaders.TRANSFER_ENCODING, "chunked").body(READY_STREAM_VALUE);
    }

    /**
     * This endpoint is used in the frontend to check if a token is valid when the SPA has been loaded for the first time
     * and there was a token stored in the client's browser.
     *
     * @param accept
     *
     * @return ResponseEntity
     */
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Successful when token is still valid.") })
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @GetMapping(value = "/token",
                produces = { MediaType.APPLICATION_JSON_VALUE,
                             MediaType.TEXT_PLAIN_VALUE })
    public ResponseEntity protectedPing(@RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        return getSuccessfulResponseEntity("pong", accept);
    }

}
