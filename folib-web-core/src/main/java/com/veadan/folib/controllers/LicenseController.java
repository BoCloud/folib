package com.veadan.folib.controllers;

import com.veadan.folib.entity.License;
import com.veadan.folib.forms.license.LicenseTableForm;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.LicenseService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.BeanUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.Objects;

/**
 * @author leipenghui
 */
@RestController
@RequestMapping("/api/license")
@Api(value = "/api/license")
public class LicenseController extends BaseController {

    @Inject
    private LicenseService licenseService;

    @ApiOperation(value = "查询license分页列表", response = LicenseTableForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('LICENSES_VIEW')")
    @GetMapping(value = "/page")
    public TableResultResponse<LicenseTableForm> page(@RequestParam(name = "page", required = false) Integer page,
                                                      @RequestParam(name = "limit", required = false) Integer limit,
                                                      @RequestParam(name = "searchKeyword", required = false) String searchKeyword) {
        return licenseService.queryLicensePage(page, limit, searchKeyword);
    }

    @ApiOperation(value = "查询license信息", response = LicenseTableForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('LICENSES_VIEW')")
    @GetMapping(value = "/detail/{licenseId}")
    public ResponseEntity<LicenseTableForm> licenseInfo(@PathVariable(name = "licenseId") String licenseId) {
        License license = licenseService.selectOneLicense(License.builder().licenseId(licenseId).build());
        LicenseTableForm licenseTableForm = null;
        if (Objects.nonNull(license)) {
            licenseTableForm = LicenseTableForm.builder().build();
            BeanUtils.copyProperties(license, licenseTableForm);
        }
        return ResponseEntity.ok(licenseTableForm);
    }
}
