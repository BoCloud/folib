package com.veadan.folib.controllers;

import com.veadan.folib.dto.scanner.*;
import com.veadan.folib.services.ArtifactWebService;
import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author veadan
 */
@RestController
@RequestMapping("/api/scanner")
@Api(description="组件扫描",tags = "组件扫描")
public class ScannerController extends BaseController {

    @Autowired
    private ArtifactWebService artifactWebService;

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping("/getCount")
    public ResponseEntity<CountDto> getCount(Authentication authentication) {
        return ResponseEntity.ok(artifactWebService.getCount(authentication));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping("/weekCount")
    public ResponseEntity<WeekCountDto> weekCount(Authentication authentication) {
        return ResponseEntity.ok(artifactWebService.weekCount(authentication));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping("/monthCount")
    public ResponseEntity<List<DayCountDto>> monthCount(Authentication authentication) {
        return ResponseEntity.ok(artifactWebService.monthCount(authentication));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping("/repositories")
    public ResponseEntity<List<RepositoryCountDto>> repositories(Authentication authentication) {
        return ResponseEntity.ok(artifactWebService.repositories(authentication));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping("/repository")
    public ResponseEntity<RepositoryScannerDto> repository(@RequestParam("storage") String storage, @RequestParam("repository") String repository, String artifactName, Integer page, Integer limit) {
        return ResponseEntity.ok(artifactWebService.repository(storage, repository, artifactName, page, limit));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/foEyesEnable")
    public ResponseEntity<Boolean> foEyesEnable() {
        return ResponseEntity.ok(artifactWebService.foEyesEnable());
    }
}
