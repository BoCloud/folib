package com.veadan.folib.controllers;

import com.veadan.folib.forms.scanner.*;
import com.veadan.folib.services.ArtifactWebService;
import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * @author leipenghui
 */
@RestController
@RequestMapping("/api/scanner")
@Api(tags = "")
public class ScannerController extends BaseController {

    @Autowired
    private ArtifactWebService artifactWebService;

    @GetMapping("/getCount")
    public ResponseEntity<CountForm> getCount(Authentication authentication) {
        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        return ResponseEntity.ok(artifactWebService.getCount(loggedUser.getUsername()));
    }

    @GetMapping("/weekCount")
    public ResponseEntity<WeekCountForm> weekCount(Authentication authentication) {
        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        return ResponseEntity.ok(artifactWebService.weekCount(loggedUser.getUsername()));
    }

    @GetMapping("/monthCount")
    public ResponseEntity<List<DayCountForm>> monthCount(Authentication authentication) {
        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        return ResponseEntity.ok(artifactWebService.monthCount(loggedUser.getUsername()));
    }

    @GetMapping("/repositories")
    public ResponseEntity<List<RepositoryCountForm>> repositories(Authentication authentication) {
        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        return ResponseEntity.ok(artifactWebService.repositories(loggedUser.getUsername()));
    }

    @GetMapping("/repository")
    public ResponseEntity<RepositoryScannerForm> repository(@RequestParam("storage") String storage, @RequestParam("repository") String repository, String artifactName, Integer page, Integer limit) {
        return ResponseEntity.ok(artifactWebService.repository(storage, repository, artifactName, page, limit));
    }
}
