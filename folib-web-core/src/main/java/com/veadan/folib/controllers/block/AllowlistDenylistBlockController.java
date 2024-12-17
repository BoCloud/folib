package com.veadan.folib.controllers.block;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.block.req.AllowlistDenylistBlockQueryReq;
import com.veadan.folib.controllers.block.req.AllowlistDenylistBlockReq;
import com.veadan.folib.domain.block.AllowlistDenylistBlockService;
import com.veadan.folib.entity.AllowlistDenylistBlock;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.enums.TagEnum;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;

/**
 * 黑白名单阻断
 */
@RestController
@RequestMapping("/api/allowlistDenylistBlock")
@Api(description = "黑白名单阻断", tags = "黑白名单阻断")
public class AllowlistDenylistBlockController extends BaseController {

    @Inject
    private AllowlistDenylistBlockService allowlistDenylistBlockService;

    @ApiOperation(value = "保存黑白名单阻断信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY, target = "'新增' + #req.category + (#req.type == 'BLACKLIST' ? '黑' : '白') + '名单:' + #req.getIdentifier()")
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @PutMapping
    public ResponseEntity<?> saveAllowlistDenylistBlock(@RequestBody AllowlistDenylistBlockReq req) {
        req.setTag(TagEnum.LATEST.toString());
        allowlistDenylistBlockService.insert(req);
        return ResponseEntity.ok().build();
    }


    @ApiOperation(value = "更新黑白名单阻断信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY, target = "'修改' + #req.category + (#req.type == 'BLACKLIST' ? '黑' : '白') + '名单:' + #req.getIdentifier()")
    @PostMapping
    public ResponseEntity<?> updateAllowlistDenylistBlock(@RequestBody AllowlistDenylistBlockReq req) {
        allowlistDenylistBlockService.update(req);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "删除黑白名单阻断信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @AuditLog(value = AuditEventNameEnum.SAFE_STRATEGY, target = "'删除' + #req.category + (#req.type == 'BLACKLIST' ? '黑' : '白') + '名单:' + #req.getIdentifier()")
    @DeleteMapping()
    public ResponseEntity<?> deleteAllowlistDenylistBlock(@RequestBody AllowlistDenylistBlockReq req) {
        allowlistDenylistBlockService.deleteAllowlistDenylistBlock(req);
        return ResponseEntity.ok().build();
    }


    @ApiOperation(value = "黑白名单阻断分页信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @GetMapping(value = "/page")
    public ResponseEntity<?> pageAllowlistDenylistBlock(AllowlistDenylistBlockQueryReq queryReq) {

        return ResponseEntity.ok( allowlistDenylistBlockService.paginQuery(queryReq));
    }

    @ApiOperation(value = "获取黑白名单阻断信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_SECURITY_POLICY')")
    @GetMapping("/info")
    public ResponseEntity<?> allowlistDenylistBlockInfo(AllowlistDenylistBlockReq req) {
        return ResponseEntity.ok(allowlistDenylistBlockService.queryAllowlistDenylistBlock(req));
    }
}
