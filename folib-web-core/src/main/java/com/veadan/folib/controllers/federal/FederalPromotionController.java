package com.veadan.folib.controllers.federal;

import com.veadan.folib.controllers.federal.req.FederalPromotionPolicyCreateReq;
import com.veadan.folib.controllers.federal.req.FederalPromotionPolicyQueryReq;
import com.veadan.folib.controllers.federal.req.FederalPromotionPolicyUpdateReq;
import com.veadan.folib.domain.policy.FederalPromotionPolicyService;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;



/**
 * @author pj
 */
@RestController
@RequestMapping("/api/federal")
@Api(description = "联邦仓库", tags = "联邦仓库")
public class FederalPromotionController {

    @Autowired
    private FederalPromotionPolicyService policyService;

    @ApiOperation(value = "新增联邦晋级策略")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PutMapping(value = "promotion/policy")
    public ResponseEntity<?> addPolicy(@RequestBody FederalPromotionPolicyCreateReq createReq) {
        SecurityContext securityContext = SecurityContextHolder.getContext();
        Authentication authentication = securityContext.getAuthentication();
        SpringSecurityUser user = (SpringSecurityUser) authentication.getPrincipal();
        String username = user.getUsername();
        createReq.setCreatedBy(username);
        createReq.setTag("latest");
        policyService.addPolicy(createReq);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "编辑联邦晋级策略")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping(value = "promotion/policy")
    public ResponseEntity<?> editPolicy(@RequestBody FederalPromotionPolicyUpdateReq updateReq) {
        SecurityContext securityContext = SecurityContextHolder.getContext();
        Authentication authentication = securityContext.getAuthentication();
        SpringSecurityUser user = (SpringSecurityUser) authentication.getPrincipal();
        String username = user.getUsername();
        updateReq.setUpdatedBy(username);
        policyService.editPolicy(updateReq);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "查看联邦晋级策略详情")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "promotion/policy/{policyId}")
    public ResponseEntity<?> detailsPolicy(@PathVariable("policyId") Long policyId) {
        return ResponseEntity.ok(policyService.policyDetail(policyId));
    }

    @ApiOperation(value = "删除联邦晋级策略详情")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @DeleteMapping(value = "promotion/policy/{policyId}")
    public ResponseEntity<?> deletePolicy(@PathVariable("policyId") Long policyId) {
        policyService.deletePolicy(policyId);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "查询联邦晋级策略列表")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "promotion/policy/list")
    public ResponseEntity<?> policyQuery(FederalPromotionPolicyQueryReq queryReq) {
        return ResponseEntity.ok(policyService.paginQuery(queryReq));
    }

    @ApiOperation(value = "重置联邦晋级策略列表")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @DeleteMapping(value = "promotion/policy/restOldData")
    public ResponseEntity<?> restOldData() {
        policyService.restOldData();
        return ResponseEntity.ok().build();
    }
}
