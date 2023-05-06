package com.veadan.folib.controllers;

import com.veadan.folib.entity.Dict;
import com.veadan.folib.forms.dict.DictForm;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.services.DictService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.Objects;

/**
 * @author leipenghui
 */
@Slf4j
@RestController
@PreAuthorize("authenticated")
@RequestMapping("/api/dict")
@Api(value = "/api/dict")
public class DictController extends BaseController {

    @Inject
    private DictService dictService;

    @ApiOperation(value = "查询最新的单个字典信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping(value = "/single")
    public ResponseEntity<Dict> getDict(Dict dict) {
        Dict dictData = dictService.selectLatestOneDict(dict);
        return ResponseEntity.ok(dictData);
    }

    @ApiOperation(value = "更新单个字典信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping(value = "/single")
    public ResponseEntity<String> updateDict(@RequestBody DictForm dict) {
        if (Objects.isNull(dict.getId()) && StringUtils.isBlank(dict.getDictKey())) {
            throw new BusinessException("参数错误");
        }
        dictService.updateDict(dict);
        return ResponseEntity.ok("ok");
    }
}
