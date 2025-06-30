package com.veadan.folib.controllers;

import com.veadan.folib.entity.Dict;
import com.veadan.folib.forms.dict.DictForm;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.DictService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 */
@Slf4j
@RestController
@RequestMapping("/api/dict")
@Api(description = "字典管理", tags = "字典管理")
public class DictController extends BaseController {

    @Inject
    private DictService dictService;

    @ApiOperation(value = "查询最新的单个字典信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("authenticated")
    @GetMapping(value = "/single")
    public ResponseEntity<Dict> getDict(Dict dict) {
        Dict dictData = dictService.selectLatestOneDict(dict);
        return ResponseEntity.ok(dictData);
    }

    @ApiOperation(value = "更新单个字典信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("authenticated")
    @PostMapping(value = "/single")
    public ResponseEntity<String> updateDict(@RequestBody DictForm dict) {
        if (Objects.isNull(dict.getId()) && StringUtils.isBlank(dict.getDictKey())) {
            throw new BusinessException("参数错误");
        }
        dictService.updateDict(dict);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "查询最新的字典列表信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("authenticated")
    @GetMapping(value = "/list")
    public ResponseEntity<List<Dict>> dictList(Dict dict) {
        return ResponseEntity.ok(dictService.selectLatestListDict(dict));
    }

    @ApiOperation(value = "删除字典信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("authenticated")
    @DeleteMapping(value = "/delete")
    public ResponseEntity<String> delete(@RequestParam("id") Long id) {
        dictService.deleteDictById(id);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "获取所有系统参数字典")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("authenticated")
    @GetMapping(value = "/system")
    public TableResultResponse<Dict> systemDict(@RequestParam(name = "page", defaultValue = "1") Integer page, @RequestParam(name = "limit", defaultValue = "10") Integer limit, @RequestParam(required = false) String dictKey) {
        return dictService.getSystemDict(page, limit, dictKey);
    }
}
