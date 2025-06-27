package com.veadan.folib.controllers;

import com.veadan.folib.domain.license.LicenseBlackWhite;
import com.veadan.folib.entity.License;
import com.veadan.folib.dto.license.LicenseTableDto;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.LicenseService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.BeanUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 */
@RestController
@RequestMapping("/api/license")
@Api(description = "license证书管理",tags = "license证书管理")
public class LicenseController extends BaseController {

    @Inject
    private LicenseService licenseService;

    @ApiOperation(value = "查询license分页列表", response = LicenseTableDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('LICENSES_VIEW')")
    @GetMapping(value = "/page")
    public TableResultResponse<LicenseTableDto> page(@RequestParam(name = "page", required = false) Integer page,
                                                     @RequestParam(name = "limit", required = false) Integer limit,
                                                     @RequestParam(name = "searchKeyword", required = false) String searchKeyword,
                                                     @RequestParam(name = "licenseId", required = false) String licenseId,
                                                     @RequestParam(name = "blackWhiteType", required = false) Integer blackWhiteType) {
        return licenseService.queryLicensePage(page, limit, searchKeyword, licenseId, blackWhiteType);
    }

    @ApiOperation(value = "查询license列表", response = LicenseTableDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('LICENSES_VIEW')")
    @GetMapping(value = "/list")
    public ResponseEntity<List<LicenseTableDto>> list(@RequestParam(name = "searchKeyword", required = false) String searchKeyword,
                                                      @RequestParam(name = "licenseId", required = false) String licenseId,
                                                      @RequestParam(name = "blackWhiteType", required = false) Integer blackWhiteType,
                                                      @RequestParam(name = "excludeBlackWhiteType", required = false) Integer excludeBlackWhiteType) {
        return ResponseEntity.ok(licenseService.queryLicense(searchKeyword, licenseId, blackWhiteType, excludeBlackWhiteType));
    }

    @ApiOperation(value = "查询license信息", response = LicenseTableDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('LICENSES_VIEW')")
    @GetMapping(value = "/detail/{licenseId}")
    public ResponseEntity<LicenseTableDto> licenseInfo(@PathVariable(name = "licenseId") String licenseId) {
        License license = licenseService.selectOneLicense(License.builder().licenseId(licenseId).build());
        LicenseTableDto licenseTableForm = null;
        if (Objects.nonNull(license)) {
            licenseTableForm = LicenseTableDto.builder().build();
            BeanUtils.copyProperties(license, licenseTableForm);
        }
        return ResponseEntity.ok(licenseTableForm);
    }

    @ApiOperation(value = "设置黑白名单")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping(value = "/blackWhite")
    public ResponseEntity<String> blackWhite(@RequestBody @Validated LicenseBlackWhite licenseBlackWhite) {
        License license = licenseService.selectOneLicense(License.builder().licenseId(licenseBlackWhite.getLicenseId()).build());
        if (Objects.isNull(license)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        }
        licenseService.blackWhite(licenseBlackWhite);
        return ResponseEntity.ok("ok");
    }
}
