package com.veadan.folib.controllers.federal;

import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.federal.req.FederalPromotionPolicyCreateReq;
import com.veadan.folib.controllers.federal.req.FederalPromotionPolicyQueryReq;
import com.veadan.folib.controllers.federal.req.FederalPromotionPolicyUpdateReq;
import com.veadan.folib.domain.policy.FederalPromotionPolicyService;
import com.veadan.folib.entity.FederalPromotionPolicy;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import javax.ws.rs.POST;
import java.util.Collections;
import java.util.Map;

/**
 * @author pj
 */
@RestController
@RequestMapping("/api/federal")
@Api(description = "联邦仓库", tags = "联邦仓库")
public class FederalPromotionController {

    @Autowired
    private FederalPromotionPolicyService policyService;

    //todo 添加权限控制

    @ApiOperation(value = "新增联邦晋级策略")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @PutMapping(value = "promotion/policy")
    public ResponseEntity<?> addPolicy(@RequestBody FederalPromotionPolicyCreateReq createReq) {
        SecurityContext securityContext = SecurityContextHolder.getContext();
        Authentication authentication = securityContext.getAuthentication();
        String username = authentication.getPrincipal().toString();
        createReq.setCreatedBy(username);
        createReq.setTag("latest");
        policyService.addPolicy(createReq);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "编辑联邦晋级策略")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @PostMapping(value = "promotion/policy")
    public ResponseEntity<?> editPolicy(@RequestBody FederalPromotionPolicyUpdateReq updateReq) {
        SecurityContext securityContext = SecurityContextHolder.getContext();
        Authentication authentication = securityContext.getAuthentication();
        String username = authentication.getPrincipal().toString();
        updateReq.setUpdatedBy(username);
        policyService.editPolicy(updateReq);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "查看联邦晋级策略详情")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @GetMapping(value = "promotion/policy/{policyId}")
    public ResponseEntity<?> detailsPolicy(@PathVariable("policyId") Long policyId) {
        return ResponseEntity.ok(policyService.policyDetail(policyId));
    }

    @ApiOperation(value = "删除联邦晋级策略详情")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @DeleteMapping(value = "promotion/policy/{policyId}")
    public ResponseEntity<?> deletePolicy(@PathVariable("policyId") Long policyId) {
        policyService.deletePolicy(policyId);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "查询联邦晋级策略列表")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @GetMapping(value = "promotion/policy/list")
    public ResponseEntity<?> policyQuery(FederalPromotionPolicyQueryReq queryReq) {
        return ResponseEntity.ok(policyService.paginQuery(queryReq));
    }

    @ApiOperation(value = "重置联邦晋级策略列表")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @DeleteMapping(value = "promotion/policy/restOldData")
    public ResponseEntity<?> restOldData() {
        policyService.restOldData();
        return ResponseEntity.ok().build();
    }
}
