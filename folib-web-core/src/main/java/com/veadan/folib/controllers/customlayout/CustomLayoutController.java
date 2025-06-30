package com.veadan.folib.controllers.customlayout;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.domain.customlayout.CustomLayoutRecord;
import com.veadan.folib.entity.CustomLayout;
import com.veadan.folib.forms.customlayout.CustomLayoutForm;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.CustomLayoutService;
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
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 */
@RestController
@RequestMapping("/api/customLayout")
@Api(description = "自定义布局", tags = "自定义布局")
public class CustomLayoutController extends BaseController {

    @Inject
    private CustomLayoutService customLayoutService;

    @ApiOperation(value = "查询自定义布局分页列表", response = CustomLayoutRecord.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping(value = "/page")
    public TableResultResponse<CustomLayoutRecord> page(@RequestParam(name = "page", required = false) Integer page,
                                                        @RequestParam(name = "limit", required = false) Integer limit,
                                                        CustomLayoutForm customLayoutForm) {
        return customLayoutService.queryCustomLayoutPage(page, limit, customLayoutForm);
    }

    @ApiOperation(value = "查询自定义布局列表", response = CustomLayoutRecord.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping(value = "/list")
    public ResponseEntity<List<CustomLayoutRecord>> list(CustomLayoutForm customLayoutForm) {
        return ResponseEntity.ok(customLayoutService.queryCustomLayoutList(customLayoutForm));
    }

    @ApiOperation(value = "查询自定义布局信息", response = CustomLayoutForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping(value = "/info")
    public ResponseEntity<Object> customLayoutInfo(CustomLayout customLayout) {
        CustomLayoutForm customLayoutForm = customLayoutService.queryCustomLayout(customLayout);
        if (Objects.isNull(customLayoutForm)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("%s自定义布局不存在", customLayout.getLayoutName()));
        }
        return ResponseEntity.ok(customLayoutForm);
    }

    @ApiOperation(value = "保存自定义布局信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PutMapping
    public ResponseEntity<String> saveCustomLayout(@RequestBody @Validated(CustomLayoutForm.SaveGroup.class) CustomLayoutForm customLayoutForm) {
        if (validateCustomLayout(customLayoutForm.getLayoutName())) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(String.format("%s自定义布局名称已存在", customLayoutForm.getLayoutName()));
        }
        customLayoutService.saveCustomLayout(customLayoutForm);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "修改自定义布局信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping
    public ResponseEntity<String> updateCustomLayout(@RequestBody @Validated(CustomLayoutForm.UpdateGroup.class) CustomLayoutForm customLayoutForm) {
        if (Objects.isNull(customLayoutForm.getId()) && !validateCustomLayout(customLayoutForm.getLayoutName())) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("%s自定义布局不存在", customLayoutForm.getLayoutName()));
        }
        ResponseEntity<String> responseEntity = validateCustomLayoutName(customLayoutForm.getId(), customLayoutForm.getLayoutName());
        if (Objects.nonNull(responseEntity)) {
            return responseEntity;
        }
        customLayoutService.updateCustomLayout(customLayoutForm);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "删除自定义布局信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @DeleteMapping
    public ResponseEntity<String> deleteCustomLayout(@RequestBody @Validated(CustomLayoutForm.DeleteGroup.class) CustomLayoutForm customLayoutForm) {
        if (!validateCustomLayout(customLayoutForm.getLayoutName())) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("%s自定义布局不存在", customLayoutForm.getLayoutName()));
        }
        customLayoutService.deleteCustomLayout(CustomLayout.builder().layoutName(customLayoutForm.getLayoutName()).build());
        return ResponseEntity.ok("ok");
    }

    private boolean validateCustomLayout(String customLayoutName) {
        CustomLayout customLayout = customLayoutService.getCustomLayout(CustomLayout.builder().layoutName(customLayoutName).build());
        return Objects.nonNull(customLayout);
    }

    private ResponseEntity<String> validateCustomLayoutName(String id, String customLayoutName) {
        if (Objects.isNull(id)) {
            return null;
        }
        CustomLayout customLayout = customLayoutService.getCustomLayout(CustomLayout.builder().id(Long.parseLong(id)).build());
        if (Objects.isNull(customLayout)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("%s自定义布局不存在", customLayoutName));
        }
        CustomLayout existsCustomLayout = customLayoutService.getCustomLayout(CustomLayout.builder().layoutName(customLayoutName).build());
        if (Objects.nonNull(existsCustomLayout) && !existsCustomLayout.getId().equals(customLayout.getId())) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("策略名称[%s]已被占用", customLayoutName));
        }
        return null;
    }

}
