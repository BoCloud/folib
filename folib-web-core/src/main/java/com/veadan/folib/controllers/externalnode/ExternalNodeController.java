package com.veadan.folib.controllers.externalnode;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.dto.externalnode.ExternalNodeDto;
import com.veadan.folib.dto.externalnode.ExternalNodeRepositoryDto;
import com.veadan.folib.dto.validate.SaveGroup;
import com.veadan.folib.dto.validate.UpdateGroup;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ExternalNodeService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.List;

/**
 * @author leipenghui
 */
@RestController
@RequestMapping("/api/externalNode")
@Api(description = "外部节点", tags = "外部节点")
public class ExternalNodeController extends BaseController {

    @Inject
    private ExternalNodeService externalNodeService;

    @ApiOperation(value = "查询外部节点分页列表", response = ExternalNodeDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_VIEW')")
    @GetMapping(value = "/page")
    public TableResultResponse<ExternalNodeDto> page(@RequestParam(name = "page", required = false) Integer page,
                                                     @RequestParam(name = "limit", required = false) Integer limit,
                                                     ExternalNodeDto externalNodeForm) {
        return externalNodeService.queryExternalNodeList(page, limit, externalNodeForm);
    }

    @ApiOperation(value = "查询外部节点信息", response = ExternalNodeDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_VIEW')")
    @GetMapping(value = "/{id}")
    public ResponseEntity<ExternalNodeDto> externalNodeInfo(@PathVariable(name = "id") Long id) {
        ExternalNodeDto externalNodeForm = externalNodeService.getExternalNode(ExternalNodeDto.builder().id(id).build());
        return ResponseEntity.ok(externalNodeForm);
    }

    @ApiOperation(value = "保存外部节点信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_SAVE')")
    @PutMapping
    public ResponseEntity<Void> saveExternalNode(@RequestBody @Validated(SaveGroup.class) ExternalNodeDto externalNodeForm) {
        externalNodeService.saveExternalNode(externalNodeForm);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "更新外部节点信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_UPDATE')")
    @PostMapping
    public ResponseEntity<Void> updateExternalNode(@RequestBody @Validated(UpdateGroup.class) ExternalNodeDto externalNodeForm) {
        externalNodeService.updateExternalNode(externalNodeForm);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "删除外部节点信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_DELETE')")
    @DeleteMapping(value = "/{id}")
    public ResponseEntity<Void> deleteExternalNode(@PathVariable(name = "id") Long id) {
        externalNodeService.deleteExternalNode(id);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "查询外部节点仓库列表", response = ExternalNodeRepositoryDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('EXTERNAL_NODE_VIEW')")
    @GetMapping(value = "/repositories")
    public ResponseEntity<List<ExternalNodeRepositoryDto>> getExternalNodeRepositories(@RequestParam(name = "type", required = false) String type) {
        return ResponseEntity.ok(externalNodeService.getExternalNodeRepositories(type));
    }
}
