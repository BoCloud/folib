package com.veadan.folib.controllers.blockstrategy;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.domain.blockstrategy.BlockStrategyRecord;
import com.veadan.folib.entity.BlockStrategy;
import com.veadan.folib.dto.blockstrategy.BlockStrategyDto;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.BlockStrategyService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.Objects;

/**
 * @author veadan
 */
@RestController
@RequestMapping("/api/blockStrategy")
@Api(description = "阻断策略", tags = "阻断策略")
public class BlockStrategyController extends BaseController {

    @Inject
    private BlockStrategyService blockStrategyService;

    @ApiOperation(value = "查询阻断策略分页列表", response = BlockStrategyRecord.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/page")
    public TableResultResponse<BlockStrategyRecord> page(@RequestParam(name = "page", required = false) Integer page,
                                                         @RequestParam(name = "limit", required = false) Integer limit,
                                                         BlockStrategyDto blockStrategyForm) {
        return blockStrategyService.queryBlockStrategyPage(page, limit, blockStrategyForm);
    }

    @ApiOperation(value = "查询阻断策略信息", response = BlockStrategyDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/info")
    public ResponseEntity<BlockStrategyDto> blockStrategyInfo(BlockStrategy blockStrategy) {
        BlockStrategyDto blockStrategyForm = blockStrategyService.queryBlockStrategy(blockStrategy);
        return ResponseEntity.ok(blockStrategyForm);
    }

    @ApiOperation(value = "保存阻断策略信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PutMapping
    public ResponseEntity<String> saveBlockStrategy(@RequestBody @Validated(BlockStrategyDto.SaveGroup.class) BlockStrategyDto blockStrategyForm) {
        if (validateBlockStrategy(blockStrategyForm.getBlockStrategyName())) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(String.format("%s阻断策略名称已存在", blockStrategyForm.getBlockStrategyName()));
        }
        blockStrategyService.saveBlockStrategy(blockStrategyForm);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "修改阻断策略信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping
    public ResponseEntity<String> updateBlockStrategy(@RequestBody @Validated(BlockStrategyDto.UpdateGroup.class) BlockStrategyDto blockStrategyForm) {
        if (Objects.isNull(blockStrategyForm.getId()) && !validateBlockStrategy(blockStrategyForm.getBlockStrategyName())) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body("未找到阻断策略信息");
        }
        ResponseEntity responseEntity = validateBlockStrategyName(blockStrategyForm.getId(), blockStrategyForm.getBlockStrategyName());
        if (Objects.nonNull(responseEntity)) {
            return responseEntity;
        }
        blockStrategyService.updateBlockStrategy(blockStrategyForm);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "删除阻断策略信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @DeleteMapping
    public ResponseEntity<String> deleteBlockStrategy(@RequestBody @Validated(BlockStrategyDto.DeleteGroup.class) BlockStrategyDto blockStrategyForm) {
        if (!validateBlockStrategy(blockStrategyForm.getBlockStrategyName())) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body("未找到阻断策略信息");
        }
        blockStrategyService.deleteBlockStrategy(BlockStrategy.builder().blockStrategyName(blockStrategyForm.getBlockStrategyName()).build());
        return ResponseEntity.ok("ok");
    }

    private boolean validateBlockStrategy(String blockStrategyName) {
        BlockStrategy blockStrategy = blockStrategyService.getBlockStrategy(BlockStrategy.builder().blockStrategyName(blockStrategyName).build());
        return Objects.nonNull(blockStrategy);
    }

    private ResponseEntity validateBlockStrategyName(String id, String blockStrategyName) {
        if (Objects.isNull(id)) {
            return null;
        }
        BlockStrategy blockStrategy = blockStrategyService.getBlockStrategy(BlockStrategy.builder().id(Long.parseLong(id)).build());
        if (Objects.isNull(blockStrategy)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body("未找到阻断策略信息");
        }
        BlockStrategy existsBlockStrategy = blockStrategyService.getBlockStrategy(BlockStrategy.builder().blockStrategyName(blockStrategyName).build());
        if (Objects.nonNull(existsBlockStrategy) && !existsBlockStrategy.getId().equals(blockStrategy.getId())) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("策略名称[%s]已被占用", blockStrategyName));
        }
        return null;
    }
}
