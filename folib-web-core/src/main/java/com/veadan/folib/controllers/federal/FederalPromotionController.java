package com.veadan.folib.controllers.federal;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.ws.rs.POST;

/**
 * @author pj
 */
@RestController
@RequestMapping("/api/federal")
@Api(description = "联邦仓库", tags = "联邦仓库")
public class FederalPromotionController {

    @ApiOperation(value = "新增联邦晋级策略")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @PutMapping(value = "promotion/policy")
    public ResponseEntity<?> addPolicy() {

        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "编辑联邦晋级策略")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @PostMapping(value = "promotion/policy")
    public ResponseEntity<?> editPolicy() {

        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "查看联邦晋级策略详情")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @GetMapping(value = "promotion/policy")
    public ResponseEntity<?> detailsPolicy() {

        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "删除联邦晋级策略详情")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @DeleteMapping(value = "promotion/policy")
    public ResponseEntity<?> deletePolicy() {

        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "查询联邦晋级策略列表")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @DeleteMapping(value = "promotion/policy")
    public ResponseEntity<?> policyQuery() {
        return ResponseEntity.ok().build();
    }
}
